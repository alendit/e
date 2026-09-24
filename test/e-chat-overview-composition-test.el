;;; e-chat-overview-composition-test.el --- Public chat overview composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Session rows, read markers, previews, workspace unread, and commands.

;;; Code:

(require 'ert)
(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-chat-test-active-session-preview-renders-detached-message-tail ()
  "Active-session preview renders only messages supplied by its bounded row."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         candidate)
    (e-chat-test--create-session store :id "indexed-active"
                      :metadata '(:name "Indexed active"))
    (e-session-append-message
     store "indexed-active"
     '(:id "msg-1" :role user :content "first prompt"))
    (e-session-append-message
     store "indexed-active"
     '(:id "msg-2" :role assistant :content "first response"))
    (e-session-append-message
     store "indexed-active"
     '(:id "msg-3" :role user :content "last prompt"))
    (e-session-append-message
     store "indexed-active"
     '(:id "msg-4" :role assistant :content "last response"))
    (let ((session (car (e-harness-session-list harness))))
      ;; Model the consumer-shaped result of the active-session query.  The
      ;; preview must use this bounded value and never inspect STORE again.
      (plist-put session :messages
                 '((:id "msg-3" :role user :content "last prompt")
                   (:id "msg-4" :role assistant :content "last response")))
      (setq candidate
            (list :harness harness
                  :session session
                  :session-id "indexed-active")))
    (let ((e-chat-resume-preview-message-limit 2))
      (with-temp-buffer
        (e-chat-overview-active-session-preview candidate (current-buffer))
        (let ((text (buffer-string)))
          (should-not (string-match-p "first prompt" text))
          (should-not (string-match-p "first response" text))
          (should (string-match-p "last prompt" text))
          (should (string-match-p "last response" text)))))))


(ert-deftest e-chat-test-overview-mode-disables-undo ()
  "Overview mode disables undo so repeated re-renders do not accrue history."
  (let ((buffer (get-buffer-create "*e-chat-overview-undo-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-overview-mode)
          (should (eq buffer-undo-list t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest e-chat-test-resume-reader-uses-consult-preview-when-available ()
  "Resume selection uses Consult preview state when Consult is available."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         selected-state selected-sort)
    (e-chat-test--create-session store :id "resume-me")
    (e-session-append-message
     store "resume-me" '(:id "msg-1" :role user :content "saved hello"))
    (let ((original-require (symbol-function 'require)))
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &optional filename noerror)
                   (if (eq feature 'consult)
                       t
                     (funcall original-require feature filename noerror))))
                ((symbol-function 'consult--read)
                 (lambda (collection &rest options)
                   (setq selected-state (plist-get options :state))
                   (setq selected-sort (plist-get options :sort))
                   (car collection))))
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
               (candidate
                (e-chat-overview-read-session-candidate candidates)))
          (should (equal (plist-get candidate :session-id) "resume-me"))
          (should (functionp selected-state))
          (should (eq selected-sort nil)))))))

(ert-deftest e-chat-test-overview-renders-sessions-in-recency-order ()
  "Overview rows render latest sessions first and mark unread sessions."
  (let* ((directory (make-temp-file "e-chat-overview-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
        (harness (e-chat-test--activate-chat-session
                  (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "older-session"
                            :metadata '(:name "Older"))
          (let ((write
                 (e-session-append-message
                  store "older-session"
                  '(:id "old-assistant" :role assistant
                    :content "older answer"))))
            (when (e-work-handle-p write)
              (e-work-with-batch-await
                (e-work-await-batch write :timeout 5.0))))
          (e-chat-test--create-session store :id "newer-session"
                            :metadata '(:name "Newer"))
          (let ((write
                 (e-session-append-message
                  store "newer-session"
                  '(:id "new-assistant" :role assistant
                    :content "newer answer"))))
            (when (e-work-handle-p write)
              (e-work-with-batch-await
                (e-work-await-batch write :timeout 5.0))))
          (let ((buffer (get-buffer-create "*e-chat-overview-test*")))
            (unwind-protect
                (with-current-buffer buffer
                  (e-chat-overview-mode)
                  (let ((candidates
                         (e-work-with-batch-await
                           (e-work-await-batch
                            (e-chat-overview-render harness) :timeout 5.0))))
                    (should
                     (equal
                      (sort (mapcar (lambda (candidate)
                                      (plist-get candidate :session-id))
                                    candidates)
                            #'string<)
                      '("newer-session" "older-session"))))
                  (let* ((text (buffer-string))
                         (newer-pos (string-match-p "Newer" text))
                         (older-pos (string-match-p "Older" text)))
                    (should newer-pos)
                    (should older-pos)
                    (should (< newer-pos older-pos))
                    (should (string-match-p "! Newer" text))
                    (should (string-match-p "! Older" text))))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))

(ert-deftest e-chat-test-overview-compacts-multiline-session-summary ()
  "Overview rows do not expand raw prompt context into the sidebar."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         (store (e-harness-sessions harness))
         (buffer (get-buffer-create "*e-chat-overview-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "messy-summary")
          (e-chat-test--await
           (e-session-append-message
            store "messy-summary"
            '(:id "messy-user"
              :role user
              :content "<reference id=\"source\" label=\"very-long-reference-name\">Ask about sidebar</reference>\n\nReferences:\n[source] plan.org")))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-test--await (e-chat-overview-render harness))
            (let ((text (buffer-string)))
              (should (string-match-p "Ask about sidebar" text))
              (should-not (string-match-p "<reference" text))
              (should-not (string-match-p "References:" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-overview-styles-session-row-regions ()
  "Overview rows style title, metadata, and summary as distinct regions."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         (store (e-harness-sessions harness))
         (buffer (get-buffer-create "*e-chat-overview-style-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "styled-session"
                            :metadata '(:name "Styled Session"))
          (e-chat-test--await
           (e-session-append-message
            store "styled-session"
            '(:id "styled-user"
              :role user
              :content "summary line"
              :created-at "2026-05-26T21:24:00Z")))
          (e-chat-test--await
           (e-session-append-message
            store "styled-session"
            '(:id "styled-assistant"
              :role assistant
              :content "answer"
              :created-at "2026-05-26T21:25:42Z")))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-test--await (e-chat-overview-render harness))
            (let ((text (buffer-string)))
              (should (string-match-p "\n\n\\'" text))
              (goto-char (point-min))
              (should (eq (get-text-property (point) 'font-lock-face)
                          'e-chat-overview-unread-face))
              (search-forward "Styled Session")
              (should (eq (get-text-property (match-beginning 0)
                                             'font-lock-face)
                          'e-chat-overview-title-face))
              (search-forward "05-26 21:25")
              (should (eq (get-text-property (match-beginning 0)
                                             'font-lock-face)
                          'e-chat-overview-meta-face))
              (search-forward "summary line")
              (should (eq (get-text-property (match-beginning 0)
                                             'font-lock-face)
                          'e-chat-overview-summary-face))
              (should-not (get-text-property (match-beginning 0)
                                             'mouse-face)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-overview-hides-summary-when-title-is-derived ()
  "Overview rows do not repeat summaries that already produced the title."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         (store (e-harness-sessions harness))
         (buffer (get-buffer-create "*e-chat-overview-duplicate-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "derived-title")
          (e-chat-test--await
           (e-session-append-message
            store "derived-title"
            '(:id "derived-user"
              :role user
              :content "this prompt is long enough to become a truncated derived title"
              :created-at "2026-05-26T21:25:42Z")))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-test--await (e-chat-overview-render harness))
            (let ((text (buffer-string)))
              (should (string-match-p "this prompt is long enoug..." text))
              (should-not (string-match-p "truncated derived title" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-overview-j-k-move-by-session-and-preview ()
  "Overview j/k navigation targets whole session rows and opens a preview."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         (store (e-harness-sessions harness))
         (buffer (get-buffer-create "*e-chat-overview-nav-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "older")
          (e-chat-test--await
           (e-session-append-message
            store "older"
            '(:id "older-user"
              :role user
              :content "older prompt"
              :created-at "2026-05-26T21:24:00Z")))
          (e-chat-test--create-session store :id "newer")
          (e-chat-test--await
           (e-session-append-message
            store "newer"
            '(:id "newer-user"
              :role user
              :content "newer prompt"
              :created-at "2026-05-26T21:25:00Z")))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-test--await (e-chat-overview-render harness))
            (goto-char (point-min))
            (should (equal (e-chat-overview-session-id-at-point) "newer"))
            (e-chat-overview-next-session)
            (should (equal (e-chat-overview-session-id-at-point) "older"))
            (with-current-buffer
                (e-chat-overview-resume-preview-buffer-name)
              (should (string-match-p "older prompt" (buffer-string))))
            (e-chat-overview-previous-session)
            (should (equal (e-chat-overview-session-id-at-point) "newer"))
            (with-current-buffer
                (e-chat-overview-resume-preview-buffer-name)
              (should (string-match-p "newer prompt" (buffer-string))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when-let* ((preview
                  (get-buffer (e-chat-overview-resume-preview-buffer-name))))
        (kill-buffer preview)))))

(ert-deftest e-chat-test-overview-renders-and-opens-owning-chat-instance ()
  "Overview rows carry owning instance metadata when session ids collide."
  (let* ((alpha-harness (e-chat-test--activate-chat-session
                         (e-harness-create
                          :backend (e-backend-fake-create :items nil))))
         (alpha-store (e-harness-sessions alpha-harness))
         (beta-harness (e-chat-test--activate-chat-session
                        (e-harness-create
                         :backend (e-backend-fake-create :items nil))))
         (beta-store (e-harness-sessions beta-harness))
         (buffer (get-buffer-create "*e-chat-overview-instances-test*")))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-alpha))
            (e-chat-test--register-chat-instance
             :chat-alpha "Alpha Target" alpha-harness t)
            (e-chat-test--register-chat-instance
             :chat-beta "Beta Target" beta-harness)
            (e-chat-test--create-session alpha-store :id "shared-session"
                              :metadata '(:name "Alpha Session"))
            (e-chat-test--create-session beta-store :id "shared-session"
                              :metadata '(:name "Beta Session"))
            (with-current-buffer buffer
              (e-chat-overview-mode)
              (e-chat-test--await (e-chat-overview-render))
              (let ((text (buffer-string)))
                (should (string-match-p "Alpha Target" text))
                (should (string-match-p "Beta Target" text)))
              (goto-char (point-min))
              (search-forward "Beta Target")
              (let ((chat-buffer (e-chat-overview-open-session)))
                (with-current-buffer chat-buffer
                  (should (eq e-chat-harness beta-harness))
                  (should (eq e-chat-harness-instance-id :chat-beta))
                  (should (equal e-chat-session-id "shared-session")))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-chat-buffers))))

(ert-deftest e-chat-test-sidebar-toggle-opens-and-closes-overview ()
  "The planned sidebar toggle command toggles the overview side window."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         (store (e-harness-sessions harness))
         (e-chat-overview-buffer-name "*e-chat-overview-toggle-test*")
         opened-buffer)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-chat-test--create-session store :id "toggle-me"
                              :metadata '(:name "Toggle Me"))
            (should (commandp 'e-chat-sidebar-toggle))
            (e-chat-sidebar-toggle)
            (setq opened-buffer (get-buffer e-chat-overview-buffer-name))
            (should (buffer-live-p opened-buffer))
            (with-current-buffer opened-buffer
              (when (e-work-handle-p e-chat-overview--page-work)
                (e-chat-test--await e-chat-overview--page-work)))
            (should (get-buffer-window opened-buffer t))
            (should (eq (window-buffer (selected-window)) opened-buffer))
            (e-chat-sidebar-toggle)
            (should-not (buffer-live-p opened-buffer))
            (should-not (get-buffer-window opened-buffer t))))
      (when (buffer-live-p opened-buffer)
        (kill-buffer opened-buffer)))))

(ert-deftest e-chat-test-active-sessions-builds-picker-spec ()
  "The active sessions command uses e-picker with chat session callbacks."
  (let* ((harness-a (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :sessions (e-session-store-create)))
         (harness-b (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :sessions (e-session-store-create)))
         (session-a '(:id "alpha-session"
                      :title "Alpha Session"
                      :summary "Alpha summary"
                      :message-count 1
                      :messages ((:id "alpha-user"
                                   :role user
                                   :content "Alpha prompt")
                                  (:id "alpha-assistant"
                                   :role assistant
                                   :content "Alpha final response"))
                      :created-at "2026-06-20T10:00:00Z"
                      :loaded t))
         (session-b '(:id "beta-session"
                      :title "Beta Session"
                      :summary "Beta summary"
                      :message-count 2
                      :latest-assistant-marker "beta-assistant"
                      :messages ((:id "beta-user"
                                   :role user
                                   :content "Beta prompt")
                                  (:id "beta-assistant"
                                   :role assistant
                                   :content "Beta final response"))
                      :created-at "2026-06-20T11:00:00Z"
                      :loaded t))
         (candidates (list (list :harness harness-a
                                 :session session-a
                                 :session-id "alpha-session")
                           (list :harness harness-b
                                 :session session-b
                                 :session-id "beta-session"
                                 :instance-id :beta)))
         spec preview-text opened)
    (cl-letf (((symbol-function 'e-chat-session-candidates-start)
               (lambda () (e-chat-test--finished-work candidates)))
              ((symbol-function 'e-chat-service-active-turn-p)
               (lambda (_harness session-id)
                 (equal session-id "alpha-session")))
              ((symbol-function 'e-context-status-text)
               (lambda (&rest _args) "ctx model/effort 10%"))
              ((symbol-function 'e-picker-open)
               (lambda (&rest args)
                 (setq spec args)
                 nil))
              ((symbol-function 'e-chat-open-session)
               (lambda (harness session-id display &optional instance-id)
                 (setq opened
                       (list :harness harness
                             :session-id session-id
                             :display display
                             :instance-id instance-id)))))
      (e-chat-active-sessions)
      (should (eq (plist-get spec :name) 'active-sessions))
      (should (= (plist-get spec :initial-candidate-limit) 15))
      (should (= (plist-get spec :candidate-limit-step) 15))
      (should (equal (funcall (plist-get spec :candidates)) candidates))
      (should (string-match-p
               "Beta Session"
               (funcall (plist-get spec :candidate-key)
                        (cadr candidates))))
      (should (string-match-p
               "ctx model/effort"
               (funcall (plist-get spec :candidate-line)
                        (cadr candidates))))
      (should (string-prefix-p
               "◆ Alpha Session"
               (funcall (plist-get spec :candidate-line)
                        (car candidates))))
      (should (string-prefix-p
               "● Beta Session"
               (funcall (plist-get spec :candidate-line)
                        (cadr candidates))))
      (should-not (string-match-p
                   "!"
                   (funcall (plist-get spec :candidate-line)
                            (cadr candidates))))
      (with-temp-buffer
        (funcall (plist-get spec :preview) (car candidates) (current-buffer))
        (setq preview-text (buffer-string))
        (should (string-match-p "Alpha prompt" preview-text))
        (should (string-match-p "Alpha final response" preview-text))
        (goto-char (point-min))
        (should (re-search-forward "Alpha final response" nil t))
        (should (memq 'e-chat-final-assistant-face
                      (ensure-list (get-text-property
                                    (match-beginning 0)
                                    'face))))
        (should-not (get-text-property (match-beginning 0) 'read-only))
        (should-not (get-text-property (match-beginning 0) 'field))
        (should-not (get-text-property (match-beginning 0) 'e-chat-block-id)))
      (funcall (plist-get spec :on-select) (cadr candidates))
      (should (eq (plist-get opened :harness) harness-b))
      (should (equal (plist-get opened :session-id) "beta-session"))
      (should (eq (plist-get opened :display) t))
      (should (eq (plist-get opened :instance-id) :beta)))))

(ert-deftest e-chat-test-active-session-line-reuses-fresh-status-snapshot ()
  "Active-session picker rows reuse fresh context-status snapshots."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (candidate (list :harness harness
                          :session '(:id "picker-status-cache"
                                      :title "Picker Status"
                                      :message-count 1
                                      :messages ((:id "picker-user"
                                                  :role user
                                                  :content "prompt"))
                                      :loaded t)
                          :session-id "picker-status-cache"))
         (status-cache (make-hash-table :test #'equal))
         (calls 0))
    (e-chat-test--create-session store :id "picker-status-cache")
    (cl-letf (((symbol-function 'e-context-budget-status)
               (lambda (&rest _args)
                 (setq calls (1+ calls))
                 '(:model "gpt-5.5"
                   :reasoning-effort "high"
                   :used-tokens 123
                   :window 1000
                   :approximate t)))
              ((symbol-function 'e-chat-overview-session-unread-p)
               (lambda (&rest _args) nil)))
      (let ((e-context-status-estimate-cache-seconds 100))
        (should (string-match-p
                 "ctx gpt-5.5/high ~13% (~123/1k tok)"
                 (e-chat-overview-active-session-line candidate status-cache)))
        (should (string-match-p
                 "ctx gpt-5.5/high ~13% (~123/1k tok)"
                 (e-chat-overview-active-session-line candidate status-cache)))))
    (should (= calls 1))))


(ert-deftest e-chat-test-active-session-preview-marks-session-read ()
  "Showing a session in the active-session preview records its latest response."
  (let* ((store (e-session-store-create))
        (harness (e-harness-create
                  :backend (e-backend-fake-create :items nil)
                  :sessions store))
        candidate)
    (e-chat-test--create-session store :id "preview-read"
                      :metadata '(:name "Preview read"))
    (e-session-append-message
     store "preview-read"
     '(:id "msg-1" :role user :content "prompt"))
    (e-session-append-message
     store "preview-read"
     '(:id "msg-2" :role assistant :content "response"))
    (setq candidate
          (list :harness harness
                :session (car (e-harness-session-list harness))
                :session-id "preview-read"))
    (should (e-chat-overview-session-unread-p
             harness
             (plist-get candidate :session)))
    (with-temp-buffer
      (e-chat-overview-active-session-preview candidate (current-buffer)))
    (should-not (e-chat-overview-session-unread-p
                 harness
                 (plist-get candidate :session)))))

(ert-deftest e-chat-test-active-sessions-errors-without-candidates ()
  "The async active sessions command reports an empty session list."
  (let (notice)
    (cl-letf (((symbol-function 'e-chat-session-candidates-start)
               (lambda () (e-chat-test--finished-work nil)))
              ((symbol-function 'message)
               (lambda (format-string &rest arguments)
                 (setq notice (apply #'format format-string arguments)))))
      (should (e-work-handle-p (e-chat-active-sessions)))
      (should (equal notice "No e chat sessions to show")))))

;;; e-chat-presentation-integration-test--end

(provide 'e-chat-overview-composition-test)

;;; e-chat-overview-composition-test.el ends here
