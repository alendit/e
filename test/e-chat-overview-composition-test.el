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

(ert-deftest e-chat-test-active-session-preview-renders-index-session-tail ()
  "Active-session preview renders a loaded index session through the chat path."
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
    (setq candidate
          (list :harness harness
                :session (car (e-harness-session-list harness))
                :session-id "indexed-active"))
    (let ((e-chat-resume-preview-message-limit 2))
      (with-temp-buffer
        (e-chat-overview-active-session-preview candidate (current-buffer))
        (let ((text (buffer-string)))
          (should-not (string-match-p "first prompt" text))
          (should-not (string-match-p "first response" text))
          (should (string-match-p "last prompt" text))
          (should (string-match-p "last response" text)))))))

(ert-deftest e-chat-test-resume-preview-for-index-session-avoids-transcript-load ()
  "Resume previews render metadata when a persistent transcript is not loaded."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get
                      (e-chat-test--create-session store
                                        :id "indexed-preview"
                                        :metadata '(:name "Indexed preview"))
                      :id))
         (backend (e-backend-fake-create :items nil))
         indexed-store)
    (unwind-protect
        (progn
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "indexed preview hello"))
          (e-session-storage-close store)
          (setq store nil
                indexed-store
                (e-session-persistent-index-store-create directory))
          (let* ((indexed-store indexed-store)
                 (harness (e-chat-test--activate-chat-session
                           (e-harness-create :backend backend
                                             :sessions indexed-store)))
                 (session (car (e-harness-session-list harness)))
                 (loaded nil))
            (should-not (plist-get session :loaded))
            (cl-letf (((symbol-function 'e-session-load-session)
                       (lambda (&rest _args)
                         (setq loaded t)
                         (error "preview loaded transcript"))))
              (let ((preview (e-chat-overview-render-resume-preview harness session)))
                (should-not loaded)
                (with-current-buffer preview
                  (let ((text (buffer-string)))
                    (should buffer-read-only)
                    (should (equal e-chat-session-id "indexed-preview"))
                    (should-not (string-match-p
                                 (regexp-quote (e-chat-composer-glyph))
                                 text))
                    (should (string-match-p "Indexed preview" text))
                    (should (string-match-p "indexed preview hello" text)))))
              (let ((preview (e-chat-overview-render-resume-preview harness session)))
                (should-not loaded)
                (with-current-buffer preview
                  (let ((text (buffer-string)))
                    (should buffer-read-only)
                    (should (equal e-chat-session-id "indexed-preview"))
                    (should-not (string-match-p
                                 (regexp-quote (e-chat-composer-glyph))
                                 text))
                    (should (string-match-p "Indexed preview" text))))))))
      (e-chat-test--kill-chat-buffers)
      (when store
        (e-session-storage-close store))
      (when indexed-store
        (e-session-storage-close indexed-store))
      (delete-directory directory t))))

(ert-deftest e-chat-test-overview-mode-disables-undo ()
  "Overview mode disables undo so repeated re-renders do not accrue history."
  (let ((buffer (get-buffer-create "*e-chat-overview-undo-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-overview-mode)
          (should (eq buffer-undo-list t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-resume-selects-existing-session ()
  "Resuming uses completing-read over persisted sessions and renders transcript."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "resume-me")
          (e-session-append-message
           store "resume-me" '(:id "msg-1" :role user :content "saved hello"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _args)
                       (car collection))))
            (e-chat-test--with-empty-harness-registry
              (let ((e-chat-default-harness-id :chat-test))
                (e-harness-registry-register :chat-test harness)
                (with-current-buffer (e-chat-resume)
                  (should (equal e-chat-session-id "resume-me"))
                  (should (string-match-p "saved hello" (buffer-string))))))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))

(ert-deftest e-chat-test-resume-selects-session-across-chat-instances ()
  "Resume candidates include sessions from every configured chat instance."
  (let* ((alpha-store (e-session-store-create))
         (beta-store (e-session-store-create))
         (alpha-harness (e-chat-test--activate-chat-session
                         (e-harness-create
                          :backend (e-backend-fake-create :items nil)
                          :sessions alpha-store)))
         (beta-harness (e-chat-test--activate-chat-session
                        (e-harness-create
                         :backend (e-backend-fake-create :items nil)
                         :sessions beta-store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-alpha))
            (e-chat-test--register-chat-instance
             :chat-alpha "Alpha Target" alpha-harness t)
            (e-chat-test--register-chat-instance
             :chat-beta "Beta Target" beta-harness)
            (e-chat-test--create-session alpha-store :id "alpha-session"
                              :metadata '(:name "Alpha Session"))
            (e-chat-test--create-session beta-store :id "beta-session"
                              :metadata '(:name "Beta Session"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (cl-find-if
                          (lambda (candidate)
                            (string-match-p "Beta Target.*Beta Session"
                                            candidate))
                          (all-completions "" collection)))))
              (with-current-buffer (e-chat-resume)
                (should (eq e-chat-harness beta-harness))
                (should (eq e-chat-harness-instance-id :chat-beta))
                (should (equal e-chat-session-id "beta-session"))))))
      (e-chat-test--kill-chat-buffers))))

(ert-deftest e-chat-test-session-candidates-deduplicate-shared-store-by-owner ()
  "Shared-store sessions appear once under their owning chat instance."
  (let* ((store (e-session-store-create))
         (alpha-harness (e-chat-test--activate-chat-session
                         (e-harness-create
                          :backend (e-backend-fake-create :items nil)
                          :sessions store)))
         (beta-harness (e-chat-test--activate-chat-session
                        (e-harness-create
                         :backend (e-backend-fake-create :items nil)
                         :sessions store))))
    (e-chat-test--with-empty-harness-registry
      (let ((e-chat-default-harness-id :chat-alpha))
        (e-chat-test--register-chat-instance
         :chat-alpha "Alpha Target" alpha-harness t)
        (e-chat-test--register-chat-instance
         :chat-beta "Beta Target" beta-harness)
        (e-chat-service-create-session
         :harness alpha-harness :id "alpha-session"
         :metadata '(:name "Alpha Session"))
        (e-chat-service-create-session
         :harness beta-harness :id "beta-session"
         :metadata '(:name "Beta Session" :harness-instance-id :chat-beta))
        (let ((candidates (e-chat-overview-session-candidates)))
          (should (= (length candidates) 2))
          (should (cl-find-if
                   (lambda (candidate)
                     (and (equal (plist-get candidate :session-id)
                                 "alpha-session")
                          (eq (plist-get candidate :instance-id)
                              :chat-alpha)))
                   candidates))
          (should (cl-find-if
                   (lambda (candidate)
                     (and (equal (plist-get candidate :session-id)
                                 "beta-session")
                          (eq (plist-get candidate :instance-id)
                              :chat-beta)))
                   candidates))
          (should-not
           (cl-find-if
            (lambda (candidate)
              (and (equal (plist-get candidate :session-id)
                          "beta-session")
                   (eq (plist-get candidate :instance-id)
                       :chat-alpha)))
            candidates)))))))

(ert-deftest e-chat-test-session-candidates-include-only-board-root-sessions ()
  "Worker, participant, and pre-board sessions stay out of chat candidates.
Private execution sessions are available through their owning board or worker
surface; switch, resume, active-sessions, and overview list only root chats."
  (let* ((store (e-session-store-create))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions store))))
    (e-chat-test--with-empty-harness-registry
      (let ((e-chat-default-harness-id :chat-alpha))
        (e-chat-test--register-chat-instance
         :chat-alpha "Alpha Target" harness t)
        (let* ((binding
                (e-chat-service-create-board
                 :harness harness :id "top-level"
                 :metadata '(:name "Top Level")))
               (board (e-chat-service-binding-board binding)))
          (e-chat-service-create-participant
           board harness :id "private-participant"
           :metadata '(:name "Private Participant")))
        (e-chat-test--create-session store :id "child-by-parent"
                          :metadata '(:name "Child"
                                      :parent-session-id "top-level"))
        (e-chat-test--create-session store :id "child-by-role"
                          :metadata '(:name "Reviewer"
                                      :subagent-role "reviewer"))
        (e-chat-test--create-session store :id "queued-task"
                          :metadata '(:name "Queue worker"
                                      :task-queue-task-id "tsk_000001"))
        (e-session-create store :id "pre-board"
                          :metadata '(:name "Unsupported old session"))
        (let ((ids (mapcar (lambda (candidate)
                             (plist-get candidate :session-id))
                           (e-chat-overview-session-candidates))))
          (should (member "top-level" ids))
          (should-not (member "private-participant" ids))
          (should-not (member "child-by-parent" ids))
          (should-not (member "child-by-role" ids))
          (should-not (member "queued-task" ids))
          (should-not (member "pre-board" ids)))))))

(ert-deftest e-chat-test-session-candidates-exclude-indexed-worker-sessions ()
  "Resume candidates classify indexed workers and private participants."
  (let* ((directory (make-temp-file "e-chat-index-candidates-" t))
         (writer (e-session-persistent-store-create directory))
         (writer-harness
          (e-chat-test--activate-chat-session
           (e-harness-create
            :backend (e-backend-fake-create :items nil)
            :sessions writer)))
         indexed-store)
    (unwind-protect
        (progn
          (let* ((binding
                  (e-chat-service-create-board
                   :harness writer-harness :id "top-level"
                   :metadata '(:name "Top Level")))
                 (board (e-chat-service-binding-board binding)))
            (e-chat-service-create-participant
             board writer-harness :id "private-participant"
             :metadata '(:name "Private Participant")))
          (e-chat-test--create-session
           writer :id "worker"
           :metadata '(:parent-session-id "top-level"
                       :subagent-role "tool-user"
                       :subagent-label "nested work"))
          (e-session-storage-close writer)
          (setq writer nil
                indexed-store
                (e-session-persistent-index-store-create directory))
          (let* ((store indexed-store)
                 (harness
                  (e-chat-test--activate-chat-session
                   (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions store))))
            (e-chat-test--with-empty-harness-registry
              (let ((e-chat-default-harness-id :chat-alpha))
                (e-chat-test--register-chat-instance
                 :chat-alpha "Alpha Target" harness t)
                (should (equal
                         (mapcar (lambda (candidate)
                                   (plist-get candidate :session-id))
                                 (e-chat-overview-session-candidates))
                         '("top-level")))))))
      (when writer
        (e-session-storage-close writer))
      (when indexed-store
        (e-session-storage-close indexed-store))
      (delete-directory directory t))))

(ert-deftest e-chat-test-session-candidates-order-newest-message-first ()
  "Switch-session candidates list newest last message first."
  (let* ((store (e-session-store-create))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions store))))
    (e-chat-test--with-empty-harness-registry
      (let ((e-chat-default-harness-id :chat-alpha))
        (e-chat-test--register-chat-instance
         :chat-alpha "Alpha Target" harness t)
        ;; Create oldest-to-newest, but message recency is the reverse of
        ;; creation order so a creation- or touch-only sort would disagree.
        (e-chat-test--create-session store :id "stale-session")
        (e-chat-test--create-session store :id "fresh-session")
        (e-chat-test--create-session store :id "middle-session")
        ;; Append newest-message session first and oldest last, so the touch
        ;; sequence runs opposite to message recency.  A sort keyed on
        ;; :updated-seq would invert the list; the message-time sort must not.
        (e-session-append-message
         store "fresh-session"
         '(:role user :content "new" :created-at "1970-01-01T01:00:00Z"))
        (e-session-append-message
         store "middle-session"
         '(:role user :content "mid" :created-at "1970-01-01T00:05:00Z"))
        (e-session-append-message
         store "stale-session"
         '(:role user :content "old" :created-at "1970-01-01T00:00:10Z"))
        (let ((ids (mapcar (lambda (candidate)
                             (plist-get candidate :session-id))
                           (e-chat-overview-session-candidates))))
          (should (equal ids
                         '("fresh-session" "middle-session"
                           "stale-session"))))))))

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
          (e-session-append-message
           store "older-session"
           '(:id "old-assistant" :role assistant :content "older answer"))
          (e-chat-test--create-session store :id "newer-session"
                            :metadata '(:name "Newer"))
          (e-session-append-message
           store "newer-session"
           '(:id "new-assistant" :role assistant :content "newer answer"))
          (let ((buffer (get-buffer-create "*e-chat-overview-test*")))
            (unwind-protect
                (with-current-buffer buffer
                  (e-chat-overview-mode)
                  (e-chat-overview-render harness)
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

(ert-deftest e-chat-test-direct-overview-renders-only-board-owning-roots ()
  "A direct-harness overview never renders a private board participant."
  (let* ((harness
          (e-chat-test--activate-chat-session
           (e-harness-create :backend (e-backend-fake-create :items nil))))
         (binding
          (e-chat-service-create-board
           :harness harness :id "overview-root"
           :metadata '(:name "Overview Owner")))
         (board (e-chat-service-binding-board binding))
         (buffer (get-buffer-create "*e-chat-overview-roots-test*")))
    (unwind-protect
        (progn
          (e-chat-service-create-participant
           board harness :id "overview-private"
           :metadata '(:name "Overview Private"))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-overview-render harness)
            (let ((text (buffer-string)))
              (should (= (e-chat-test--count-occurrences
                          "Overview Owner" text)
                         1))
              (should-not (string-match-p "Overview Private" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-overview-compacts-multiline-session-summary ()
  "Overview rows do not expand raw prompt context into the sidebar."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (buffer (get-buffer-create "*e-chat-overview-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "messy-summary")
          (e-session-append-message
           store "messy-summary"
           '(:id "messy-user"
             :role user
             :content "<reference id=\"source\" label=\"very-long-reference-name\">Ask about sidebar</reference>\n\nReferences:\n[source] plan.org"))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-overview-render harness)
            (let ((text (buffer-string)))
              (should (string-match-p "Ask about sidebar" text))
              (should-not (string-match-p "<reference" text))
              (should-not (string-match-p "References:" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-overview-styles-session-row-regions ()
  "Overview rows style title, metadata, and summary as distinct regions."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (buffer (get-buffer-create "*e-chat-overview-style-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "styled-session"
                            :metadata '(:name "Styled Session"))
          (e-session-append-message
           store "styled-session"
           '(:id "styled-user"
             :role user
             :content "summary line"
             :created-at "2026-05-26T21:24:00Z"))
          (e-session-append-message
           store "styled-session"
           '(:id "styled-assistant"
             :role assistant
             :content "answer"
             :created-at "2026-05-26T21:25:42Z"))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-overview-render harness)
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
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (buffer (get-buffer-create "*e-chat-overview-duplicate-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "derived-title")
          (e-session-append-message
           store "derived-title"
           '(:id "derived-user"
             :role user
             :content "this prompt is long enough to become a truncated derived title"
             :created-at "2026-05-26T21:25:42Z"))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-overview-render harness)
            (let ((text (buffer-string)))
              (should (string-match-p "this prompt is long enoug..." text))
              (should-not (string-match-p "truncated derived title" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-overview-j-k-move-by-session-and-preview ()
  "Overview j/k navigation targets whole session rows and opens a preview."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (buffer (get-buffer-create "*e-chat-overview-nav-test*")))
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "older")
          (e-session-append-message
           store "older"
           '(:id "older-user"
             :role user
             :content "older prompt"
             :created-at "2026-05-26T21:24:00Z"))
          (e-chat-test--create-session store :id "newer")
          (e-session-append-message
           store "newer"
           '(:id "newer-user"
             :role user
             :content "newer prompt"
             :created-at "2026-05-26T21:25:00Z"))
          (with-current-buffer buffer
            (e-chat-overview-mode)
            (e-chat-overview-render harness)
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
      (when-let ((preview
                  (get-buffer (e-chat-overview-resume-preview-buffer-name))))
        (kill-buffer preview)))))

(ert-deftest e-chat-test-overview-renders-and-opens-owning-chat-instance ()
  "Overview rows carry owning instance metadata when session ids collide."
  (let* ((alpha-store (e-session-store-create))
         (beta-store (e-session-store-create))
         (alpha-harness (e-chat-test--activate-chat-session
                         (e-harness-create
                          :backend (e-backend-fake-create :items nil)
                          :sessions alpha-store)))
         (beta-harness (e-chat-test--activate-chat-session
                        (e-harness-create
                         :backend (e-backend-fake-create :items nil)
                         :sessions beta-store)))
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
              (e-chat-overview-render)
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

(ert-deftest e-chat-test-overview-deduplicates-shared-store-by-owner ()
  "Overview rows show shared-store sessions only under their owning instance."
  (let* ((store (e-session-store-create))
         (alpha-harness (e-chat-test--activate-chat-session
                         (e-harness-create
                          :backend (e-backend-fake-create :items nil)
                          :sessions store)))
         (beta-harness (e-chat-test--activate-chat-session
                        (e-harness-create
                         :backend (e-backend-fake-create :items nil)
                         :sessions store)))
         (buffer (get-buffer-create "*e-chat-overview-shared-store-test*")))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-alpha))
            (e-chat-test--register-chat-instance
             :chat-alpha "Alpha Target" alpha-harness t)
            (e-chat-test--register-chat-instance
             :chat-beta "Beta Target" beta-harness)
            (e-chat-service-create-session
             :harness alpha-harness :id "alpha-session"
             :metadata '(:name "Alpha Session"))
            (e-chat-service-create-session
             :harness beta-harness :id "beta-session"
             :metadata '(:name "Beta Session"
                         :harness-instance-id :chat-beta))
            (with-current-buffer buffer
              (e-chat-overview-mode)
              (e-chat-overview-render)
              (let ((text (buffer-string)))
                (should (= (e-chat-test--count-occurrences
                            "Alpha Session" text)
                           1))
                (should (= (e-chat-test--count-occurrences
                            "Beta Session" text)
                           1))
                (should (string-match-p "Alpha Target.*Alpha Session" text))
                (should (string-match-p "Beta Target.*Beta Session" text))
                (should-not
                 (string-match-p "Alpha Target.*Beta Session" text))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-chat-buffers))))

(ert-deftest e-chat-test-sidebar-toggle-opens-and-closes-overview ()
  "The planned sidebar toggle command toggles the overview side window."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
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
    (cl-letf (((symbol-function 'e-chat-overview-active-session-candidates)
               (lambda () candidates))
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

(ert-deftest e-chat-test-active-session-preview-avoids-unloaded-index-session-load ()
  "Active-session preview renders metadata for unloaded index sessions."
  (let* ((directory (make-temp-file "e-chat-active-" t))
         (store (e-session-persistent-store-create directory))
         (e-chat-session-summary-preview-max-chars 6)
         loaded
         indexed-store)
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "unloaded-active"
                            :metadata '(:name "Unloaded active"))
          (e-session-append-message
           store "unloaded-active"
           '(:id "msg-1" :role user :content "last prompt"))
          (e-session-append-message
           store "unloaded-active"
           '(:id "msg-2" :role assistant :content "last response"))
          (e-session-storage-close store)
          (setq store nil
                indexed-store
                (e-session-persistent-index-store-create directory))
          (let* ((indexed-store indexed-store)
                 (harness (e-harness-create
                           :backend (e-backend-fake-create :items nil)
                           :sessions indexed-store))
                 (session (car (e-harness-session-list harness)))
                 (candidate
                  (list :harness harness
                        :session session
                        :session-id "unloaded-active")))
            (should-not (plist-get session :loaded))
            (cl-letf (((symbol-function 'e-session-load-session)
                       (lambda (&rest _args)
                         (setq loaded t)
                         (error "preview loaded transcript"))))
              (with-temp-buffer
                (e-chat-overview-active-session-preview candidate (current-buffer))
                (let ((text (buffer-string)))
                  (should-not loaded)
                  (should (string-match-p "last p…" text))
                  (should-not (string-match-p "last prompt" text))
                  (should-not (string-match-p "last response" text)))))))
      (when store
        (e-session-storage-close store))
      (when indexed-store
        (e-session-storage-close indexed-store))
      (delete-directory directory t))))

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
  "The active sessions command reports an empty session list."
  (cl-letf (((symbol-function 'e-chat-overview-active-session-candidates)
             (lambda () nil)))
    (should-error (e-chat-active-sessions) :type 'user-error)))

;;; e-chat-presentation-integration-test--end

(provide 'e-chat-overview-composition-test)

;;; e-chat-overview-composition-test.el ends here
