;;; e-chat-transcript-composition-test.el --- Public chat transcript composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Transcript output, navigation, structured blocks, replay, and read-only view.

;;; Code:

(require 'ert)
(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)
(load (expand-file-name
       "e-test-environment-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(ert-deftest e-chat-test-after-display-clears-chat-navigation-modes ()
  "Displaying chat returns it to a plain composer input state."
  (let ((buffer (e-chat-test--buffer nil "chat-display-input-state")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (e-chat-tool-list-mode 1)
          (e-chat-after-display-buffer buffer)
          (should-not e-chat-tool-list-mode)
          (should-not e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-point-in-composer-p))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-loaded-session-reprojection-does-not-tail-scrollback ()
  "Session replay does not tail a transcript physically showing scrollback."
  (let* ((history (mapconcat (lambda (number)
                               (format "settled history line %d" number))
                             (number-sequence 1 300)
                             "\n"))
         (buffer (e-chat-test--buffer nil "chat-loaded-scrollback"))
         transcript-window
         composer-window)
    (unwind-protect
        (progn
          (setq transcript-window (display-buffer buffer))
          (with-current-buffer buffer
            (let ((store (e-chat-service-session-store e-chat-harness)))
              (e-session-append-message
               store e-chat-session-id
               '(:id "msg-1" :role user :content "loaded question"))
              (e-session-append-message
               store e-chat-session-id
               `(:id "msg-2" :role assistant :content ,history))
              (e-chat-test--seed-board-log-from-private-fixture
               e-chat-harness e-chat-session-id))
            (e-chat-attach-buffer
             buffer e-chat-harness e-chat-session-id
             e-chat-harness-instance-id)
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window t))
            (set-buffer buffer)
            (e-chat-surface-show-latest-output transcript-window)
            (should (e-chat-surface-window-follows-output-p transcript-window))
            ;; `scroll-other-window' and host restoration can move the paired
            ;; transcript while leaving its composer selected.  The stored
            ;; live-output flag is intentionally not consulted by a full
            ;; projection replacement; the physical pre-replay viewport is
            ;; the complete fact that operation needs.
            (set-window-point transcript-window (point-min))
            (set-window-start transcript-window (point-min))
            (redisplay t)
            (should (eq (selected-window) composer-window))
            (should (= (window-point transcript-window) (point-min)))
            (e-chat-attach-buffer
             buffer e-chat-harness e-chat-session-id
             e-chat-harness-instance-id)
            (should (< (window-point transcript-window) (point-max)))
            (should (= (window-start transcript-window) (point-min)))))
      (when (window-live-p composer-window)
        (delete-window composer-window))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-details-shows-intermittent-events ()
  "Details buffer shows intermittent reasoning before metadata."
  (let ((buffer (e-chat-test--buffer nil "chat-intermittent-expand")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type reasoning-delta
                                      :content "Need current buffer state.")))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (e-chat-render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:reason stop)))
          (call-interactively #'e-chat-enter-response-navigation)
          (let ((details (e-chat-response-navigation-details)))
            (with-current-buffer details
              (should (string-match-p
                       "Reasoning\n  Need current buffer state\\.\n\n  Turn: turn-1"
                       (buffer-string))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-buffer-name e-chat-details-buffer-name))))

(ert-deftest e-chat-test-open-loaded-session-renders-initial-tail ()
  "Opening a large loaded session renders a tail plus omitted-history marker."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-chat-session-replay-message-limit 2)
         buffer)
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "loaded-tail"
                            :metadata '(:name "Loaded tail"))
          (dolist (message
                   '((:id "msg-1" :role user :content "first prompt")
                     (:id "msg-2" :role assistant :content "first response")
                     (:id "msg-3" :role user :content "middle prompt")
                     (:id "msg-4" :role user :content "last prompt")
                     (:id "msg-5" :role assistant :content "last response")))
            (e-session-append-message store "loaded-tail" message))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "loaded-tail")
          (setq buffer (e-chat-open-session harness "loaded-tail"))
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              ;; A loaded session renders directly; the observable contract
              ;; is that no asynchronous loading placeholder remains.
              (should-not (string-match-p "Loading transcript" text))
              (should (string-match-p
                       "3 earlier transcript messages omitted" text))
              (should-not (string-match-p "first prompt" text))
              (should-not (string-match-p "middle prompt" text))
              (should (string-match-p "last prompt" text))
              (should (string-match-p "last response" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-turns-and-responses-have-stable-separators ()
  "Rendered turns use explicit separator text outside navigable blocks."
  (let ((buffer (e-chat-test--buffer nil "chat-turn-separators")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 21 "second" "two")
          (should (stringp (e-chat-transcript-turn-separator)))
          (should (stringp (e-chat-transcript-response-separator)))
          (let ((content (buffer-string)))
            (should (= (e-chat-test--count-occurrences
                        (e-chat-transcript-turn-separator) content)
                       1))
            (should (= (e-chat-test--count-font-lock-face-runs
                        'e-chat-separator-face (point-min) (point-max))
                       2))
            (should (equal (e-chat-transcript-response-separator)
                           (e-chat-composer-separator)))
            (should (string-match-p
                     (concat "one\\(.\\|\n\\)*"
                             (regexp-quote (e-chat-transcript-turn-separator))
                             "\\(.\\|\n\\)*"
                             (regexp-quote (e-chat-transcript-user-glyph))
                             " second")
                     content)))
          (goto-char (point-min))
          (search-forward (e-chat-transcript-turn-separator))
          (should (eq (get-text-property (line-beginning-position)
                                         'font-lock-face)
                      'e-chat-turn-separator-face))
          (should (get-text-property (line-beginning-position) 'read-only))
          (should-not (get-text-property (line-beginning-position)
                                         'e-chat-block-id))
          (goto-char (point-min))
          (search-forward (e-chat-transcript-response-separator))
          (should (eq (get-text-property (line-beginning-position)
                                         'font-lock-face)
                      'e-chat-separator-face))
          (should (get-text-property (line-beginning-position) 'read-only))
          (should-not (get-text-property (line-beginning-position)
                                         'e-chat-block-id)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-navigation-excludes-separators-and-does-not-reflow ()
  "Block navigation changes focus without adding/removing separator text."
  (let ((buffer (e-chat-test--buffer nil "chat-navigation-separator-stability")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 21 "second" "two")
          (should (stringp (e-chat-transcript-turn-separator)))
          (should (stringp (e-chat-transcript-response-separator)))
          (let ((content-before (buffer-string))
                (lines-before (count-lines (point-min) (point-max))))
            (goto-char (point-min))
            (search-forward "two")
            (call-interactively #'e-chat-enter-response-navigation)
            (should-not (string-match-p
                         (regexp-quote (e-chat-transcript-turn-separator))
                         (e-chat-test--focused-turn-text)))
            (should-not (string-match-p
                         (regexp-quote (e-chat-transcript-response-separator))
                         (e-chat-test--focused-turn-text)))
            (call-interactively
             (lookup-key e-chat-response-navigation-mode-map (kbd "k")))
            (should-not (string-match-p
                         (regexp-quote (e-chat-transcript-turn-separator))
                         (e-chat-test--focused-turn-text)))
            (should-not (string-match-p
                         (regexp-quote (e-chat-transcript-response-separator))
                         (e-chat-test--focused-turn-text)))
            (should (equal (buffer-string) content-before))
            (should (= (count-lines (point-min) (point-max))
                       lines-before))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-navigation-reveal-hidden-shows-and-focuses ()
  "Pressing `h' in navigation mode reveals hidden messages as focusable blocks.
The superseded first attempt and the machine-authored corrective prompt are
hidden from the clean transcript, but the ESC inspection mode must expose them
on demand so the user can audit what the calibration follow-up removed."
  (let ((buffer (e-chat-test--buffer nil "chat-hidden-reveal")))
    (unwind-protect
        (with-current-buffer buffer
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-reveal"
           (list :id "m-visible" :role 'assistant :turn-id "turn-1"
                 :content "revised answer"))
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-reveal"
           (list :id "m-first" :role 'assistant :turn-id "turn-1"
                 :content "superseded first attempt" :display 'hidden))
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-reveal"
           (list :id "m-prompt" :role 'user :turn-id "turn-1"
                 :content "machine corrective prompt"
                 :metadata '(:display hidden)))
          (e-chat-test--seed-board-log-from-private-fixture
           e-chat-harness e-chat-session-id)
          (e-chat-clear)
          (e-chat-transcript-render-session)
          (should (e-chat-test--message-display-hidden-p "m-first"))
          (should (e-chat-test--message-display-hidden-p "m-prompt"))
          (e-chat-test--focus-block-containing "revised answer")
          (should-not (e-chat-transcript-reveal-hidden-p))
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "h")))
          (should (e-chat-transcript-reveal-hidden-p))
          (should (string-match-p "superseded first attempt" (buffer-string)))
          (should (string-match-p "machine corrective prompt" (buffer-string)))
          ;; A revealed hidden message is a real navigable block.
          (e-chat-test--focus-block-containing "superseded first attempt")
          (should (eq (plist-get (e-chat-test--focused-block) :kind)
                      'hidden))
          (should e-chat-response-navigation-mode))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-navigation-reveal-hidden-toggles-off ()
  "Pressing `h' twice hides the revealed messages again.
Reveal is a temporary inspection affordance; toggling it off restores the clean
one-answer transcript."
  (let ((buffer (e-chat-test--buffer nil "chat-hidden-reveal-off")))
    (unwind-protect
        (with-current-buffer buffer
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-reveal-off"
           (list :id "m-visible" :role 'assistant :turn-id "turn-1"
                 :content "revised answer"))
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-reveal-off"
           (list :id "m-first" :role 'assistant :turn-id "turn-1"
                 :content "superseded first attempt" :display 'hidden))
          (e-chat-test--seed-board-log-from-private-fixture
           e-chat-harness e-chat-session-id)
          (e-chat-clear)
          (e-chat-transcript-render-session)
          (e-chat-test--focus-block-containing "revised answer")
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "h")))
          (should (string-match-p "superseded first attempt" (buffer-string)))
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "h")))
          (should-not (e-chat-transcript-reveal-hidden-p))
          (should (e-chat-test--message-display-hidden-p "m-first")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-assistant-markdown-renders-with-text-properties ()
  "Assistant messages keep Markdown text and use markdown-mode faces."
  (e-test-require-feature 'markdown-mode 'markdown-mode)
  (let ((buffer (e-chat-test--buffer nil "chat-markdown"))
        (markdown-mode-hook-ran nil)
        (markdown-mode-hook
         (list (lambda () (setq markdown-mode-hook-ran t)))))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-transcript-insert-entry
           "Assistant"
           "## Heading\nUse **bold**, *italic*, and `code`.\n- item\n\n```elisp\n(message \"hi\")\n```\n\n[docs](https://example.test)")
          (should-not markdown-mode-hook-ran)
          (let ((content (buffer-string)))
            (should (string-match-p "## Heading" content))
            (should (string-match-p "\\*\\*bold\\*\\*" content))
            (should (string-match-p "`code`" content))
            (should (string-match-p "```elisp" content))
            (should (string-match-p "\\[docs\\](https://example.test)" content)))
          (save-excursion
            (goto-char (point-min))
            (search-forward "##")
            (should-not (get-text-property (1- (point)) 'invisible))
            (should (memq 'markdown-header-delimiter-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-forward "Heading")
            (should (memq 'markdown-header-face-2
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (should-not (eq (get-text-property (1- (point)) 'font-lock-face)
                            'e-chat-assistant-face))
            (search-forward "bold")
            (should (memq 'markdown-bold-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-backward "**")
            (should (memq 'markdown-markup-face
                          (ensure-list (get-text-property (point) 'face))))
            (should-not (get-text-property (point) 'invisible))
            (search-forward "italic")
            (should (memq 'markdown-italic-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-backward "*")
            (should (memq 'markdown-markup-face
                          (ensure-list (get-text-property (point) 'face))))
            (should-not (get-text-property (point) 'invisible))
            (search-forward "code")
            (should (memq 'markdown-inline-code-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-backward "`")
            (should (memq 'markdown-markup-face
                          (ensure-list (get-text-property (point) 'face))))
            (should-not (get-text-property (point) 'invisible))
            (search-forward "- item")
            (search-backward "-")
            (should (memq 'markdown-list-face
                          (ensure-list (get-text-property (point) 'face))))
            (search-forward "```elisp")
            (should (memq 'markdown-code-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (should-not (get-text-property (1- (point)) 'invisible))
            (search-forward "(message \"hi\")")
            (should (memq 'markdown-code-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-forward "```")
            (should (memq 'markdown-code-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-forward "docs")
            (should (seq-some
                     (lambda (face)
                       (memq face '(markdown-link-face markdown-markup-face)))
                     (ensure-list
                      (get-text-property (1- (point)) 'face))))
            (should (equal (buffer-substring-no-properties
                            (- (point) (length "docs")) (point))
                           "docs"))
            (should (equal (get-text-property (1- (point)) 'help-echo)
                           "https://example.test"))
            (should (equal (get-text-property (1- (point)) 'e-chat-link-url)
                           "https://example.test"))
            (search-backward "[")
            (should (seq-some
                     (lambda (face)
                       (memq face '(markdown-link-face markdown-markup-face)))
                     (ensure-list (get-text-property (point) 'face))))
            (should-not (get-text-property (point) 'invisible))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-assistant-markdown-fallback-keeps-exact-link-target ()
  "The no-markdown-mode renderer retains its visible label and exact target."
  (let ((buffer (e-chat-test--buffer nil "chat-markdown-fallback"))
        (original-require (symbol-function 'require)))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &rest args)
                       (unless (eq feature 'markdown-mode)
                         (apply original-require feature args)))))
            (e-chat-transcript-insert-entry
             "Assistant" "See [docs](https://example.test)."))
          (save-excursion
            (goto-char (point-min))
            (search-forward "docs")
            (should (equal (buffer-substring-no-properties
                            (- (point) (length "docs")) (point))
                           "docs"))
            (should (equal (get-text-property (1- (point)) 'e-chat-link-url)
                           "https://example.test"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-assistant-org-output-mode-renders-org-links ()
  "With output mode `org' the renderer uses Org faces and clickable Org links."
  (let ((buffer (e-chat-test--buffer nil "chat-org-output")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-output-mode-session-set
           e-chat-harness e-chat-session-id 'org)
          (should (eq (e-chat-output-mode-resolve
                       e-chat-harness e-chat-session-id)
                      'org))
          (e-chat-transcript-insert-entry
           "Assistant"
           "* Heading
See [[https://example.test][docs]] and [[file:notes.org]].")
          (save-excursion
            (goto-char (point-min))
            (search-forward "docs")
            (should (memq 'e-chat-markdown-link-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (should (equal (get-text-property (1- (point)) 'e-chat-link-url)
                           "https://example.test"))
            ;; The [[...][ bracket syntax around the description is concealed.
            (search-backward "[[https")
            (should (get-text-property (point) 'invisible))
            (goto-char (point-min))
            (search-forward "notes.org")
            (should (equal (get-text-property (1- (point)) 'e-chat-link-url)
                           "file:notes.org"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-resource-link-opens-read-only-buffer ()
  "Clicking a session resource link opens its content read-only."
  (let ((buffer (e-chat-test--buffer nil "chat-resource-link"))
        opened-uri
        opened-buffer)
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-output-mode-session-set
           e-chat-harness e-chat-session-id 'org)
          (e-chat-transcript-insert-entry
           "Assistant"
           "See [[session://e/sessions/session-1/messages][discussion]].")
          (goto-char (point-min))
          (search-forward "discussion")
          (cl-letf (((symbol-function 'e-resources-read)
                     (lambda (_registry uri &optional _range)
                       (setq opened-uri uri)
                       "resource body")))
            (setq opened-buffer (e-chat-open-link)))
          (should (equal opened-uri
                         "session://e/sessions/session-1/messages"))
          (should (buffer-live-p opened-buffer))
          (with-current-buffer opened-buffer
            (should (derived-mode-p 'special-mode))
            (should buffer-read-only)
            (should (equal (buffer-string) "resource body"))))
      (when (buffer-live-p opened-buffer)
        (kill-buffer opened-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-set-output-mode-rerenders-visible-blocks ()
  "Toggling output mode re-renders visible final assistant blocks."
  (let ((buffer (e-chat-test--buffer nil "chat-output-toggle")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-transcript-insert-entry
           "Assistant"
           "See [[https://example.test][docs]]."
           nil
           "turn-org-toggle")
          ;; Default markdown mode leaves the Org link text literal.
          (save-excursion
            (goto-char (point-min))
            (search-forward "docs")
            (should-not (get-text-property (1- (point)) 'e-chat-link-url)))
          (e-chat-set-output-mode 'org)
          (save-excursion
            (goto-char (point-min))
            (search-forward "docs")
            (should (equal (get-text-property (1- (point)) 'e-chat-link-url)
                           "https://example.test"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-final-response-uses-visual-marker-face ()
  "Final assistant output is visually distinguished without a text label."
  (let ((buffer (e-chat-test--buffer nil "chat-final-face")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (goto-char (point-min))
          (search-forward "Final answer.")
          (should (memq 'e-chat-final-assistant-face
                        (ensure-list
                         (get-text-property (1- (point)) 'face))))
          (should-not (string-match-p "\nFinal\n" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-final-response-keeps-markdown-faces ()
  "Settled assistant styling preserves Markdown presentation faces."
  (let ((buffer (e-chat-test--buffer nil "chat-final-markdown")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-transcript-insert-entry
           "Assistant"
           "Use **bold** and `code`."
           nil
           "turn-final-md")
          (goto-char (point-min))
          (search-forward "bold")
          (let ((faces (ensure-list
                        (get-text-property (1- (point)) 'face))))
            (should (memq 'e-chat-final-assistant-face faces))
            (should (seq-some
                     (lambda (face)
                       (memq face '(e-chat-markdown-strong-face
                                    markdown-bold-face)))
                     faces))
            (should-not (eq (get-text-property (1- (point)) 'font-lock-face)
                            'e-chat-final-assistant-face)))
          (search-forward "code")
          (let ((faces (ensure-list
                        (get-text-property (1- (point)) 'face))))
            (should (memq 'e-chat-final-assistant-face faces))
            (should (seq-some
                     (lambda (face)
                       (memq face '(e-chat-markdown-code-face
                                    markdown-inline-code-face)))
                     faces))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-j-and-k-move-focus ()
  "Response navigation j/k move between turn blocks."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-move")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 23.5 "second" "two")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "k")))
          (should (string-match-p
                   (concat (regexp-quote (e-chat-transcript-user-glyph)) " second")
                   (e-chat-test--focused-turn-text)))
          (should-not (string-match-p
                       (concat (regexp-quote (e-chat-transcript-assistant-glyph)) " two")
                       (e-chat-test--focused-turn-text)))
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "j")))
          (should (string-match-p
                   (concat (regexp-quote (e-chat-transcript-assistant-glyph)) " two")
                   (e-chat-test--focused-turn-text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-ret-enters-block-view ()
  "RET on a final block enters block-local view and ESC returns to navigation."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-expand")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 23.5 "second" "two")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (should (equal (plist-get (e-chat-test--focused-block) :action-text)
                         "two"))
          (call-interactively #'e-chat-block-view-back)
          (should-not e-chat-block-view-mode)
          (should e-chat-response-navigation-mode)
          (should (equal (e-chat-transcript-focused-turn-id) "turn-2")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-block-view-can-select-and-copy-text ()
  "Block view v starts a region, h/l keep it active, and y copies it."
  (let ((buffer (e-chat-test--buffer nil "chat-block-view-select")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "alpha beta")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "v")))
          (dotimes (_ 5)
            (call-interactively
             (lookup-key e-chat-block-view-mode-map (kbd "l"))))
          (should (region-active-p))
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "y")))
          (should (equal (current-kill 0) "alpha")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-block-view-esc-clears-selection-before-exiting ()
  "In block-view selection mode, ESC resets selection before returning to nav."
  (let ((buffer (e-chat-test--buffer nil "chat-block-view-selection-esc")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "alpha beta")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "v")))
          (dotimes (_ 5)
            (call-interactively
             (lookup-key e-chat-block-view-mode-map (kbd "l"))))
          (should (region-active-p))
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "<escape>")))
          (should-not (region-active-p))
          (should e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "<escape>")))
          (should-not e-chat-block-view-mode)
          (should e-chat-response-navigation-mode))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-copy-and-open-use-block-content ()
  "Copy and open actions use the focused block action text without chrome."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-actions"))
        (opened nil))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first prompt" "final text")
          (e-chat-test--focus-block-containing "first prompt")
          (should (eq (plist-get (e-chat-test--focused-block) :kind) 'user))
          (call-interactively #'e-chat-response-navigation-copy)
          (should (equal (current-kill 0) "first prompt"))
          (setq opened (e-chat-response-navigation-open))
          (with-current-buffer opened
            (should (derived-mode-p 'text-mode))
            (should-not buffer-read-only)
            (should (equal (buffer-string) "first prompt")))
          (with-current-buffer buffer
            (e-chat-test--focus-block-containing "final text")
            (should (eq (plist-get (e-chat-test--focused-block) :kind) 'final))
            (call-interactively #'e-chat-response-navigation-copy)
            (should (equal (current-kill 0) "final text"))))
      (when (buffer-live-p opened)
        (kill-buffer opened))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-replayed-session-uses-synthetic-turns ()
  "Replayed messages without turn metadata remain navigable."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-nav-replay")
          (e-session-append-message
           store "chat-nav-replay"
           '(:role user
             :content "old first"
             :created-at "1970-01-01T00:00:10Z"))
          (e-session-append-message
           store "chat-nav-replay"
           '(:role assistant
             :content "old one"
             :created-at "1970-01-01T00:00:12Z"))
          (e-session-append-message
           store "chat-nav-replay"
           '(:role user
             :content "old second"
             :created-at "1970-01-01T00:00:20Z"))
          (e-session-append-message
           store "chat-nav-replay"
           '(:role assistant
             :content "old two"
             :created-at "1970-01-01T00:00:22Z"))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "chat-nav-replay")
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-nav-replay"))
          (with-current-buffer buffer
            (call-interactively #'e-chat-enter-response-navigation)
            (should
             (equal (e-chat-transcript-focused-turn-id)
                    (plist-get
                     (seq-find
                      (lambda (message)
                        (equal (plist-get message :content) "old second"))
                      (e-chat-service-messages harness "chat-nav-replay"))
                     :turn-id)))
            (let ((details (e-chat-response-navigation-details)))
              (with-current-buffer details
                (should (string-match-p
                         "  Started: 1970-01-01T00:00:20Z"
                         (buffer-string)))
                (should (string-match-p
                         "  Ended: 1970-01-01T00:00:22Z"
                         (buffer-string)))
                (should (string-match-p "  Duration: 0min 2sec"
                                        (buffer-string)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-buffer-name e-chat-details-buffer-name))))

(ert-deftest e-chat-test-replay-render-never-reads-private-transcript-indexes ()
  "Opening durable history renders only the board-derived projection."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         buffer)
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "board-only-render")
          (e-session-append-message
           store "board-only-render"
           '(:id "private-user" :role user :content "board prompt"))
          (e-session-append-message
           store "board-only-render"
           '(:id "private-output" :role assistant :content "board answer"))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "board-only-render")
          (setq buffer (e-chat-open :harness harness
                                    :session-id "board-only-render"))
          (cl-letf (((symbol-function 'e-harness-messages)
                     (lambda (&rest _) (error "private transcript read")))
                    ((symbol-function 'e-session-messages)
                     (lambda (&rest _) (error "private message index read")))
                    ((symbol-function 'e-session-activity-events)
                     (lambda (&rest _) (error "private activity index read"))))
            (with-current-buffer buffer
              (e-chat-clear)
              (e-chat-transcript-render-session)
              (should (string-match-p "board prompt" (buffer-string)))
              (should (string-match-p "board answer" (buffer-string))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-add-context-clears-target-block-view-mode ()
  "Context insertion into a chat target exits stale block view state."
  (let ((buffer (e-chat-test--buffer nil "chat-context-block-view"))
        (reference '(:uri "buffer://source"
                     :label "source:1"
                     :text "source"
                     :start-line 1
                     :end-line 1
                     :point-line 1)))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-block-view-mode)
          (e-chat-add-context-reference-to-session
           reference
           e-chat-harness
           e-chat-session-id)
          (should-not e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-point-in-composer-p)))
          (should-not (eq (key-binding (kbd "h") t)
                          #'e-chat-block-view-left)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-add-context-clears-target-response-navigation-mode ()
  "Context insertion into a chat target exits stale response navigation state."
  (let ((buffer (e-chat-test--buffer nil "chat-context-response-nav"))
        (reference '(:uri "buffer://source"
                     :label "source:1"
                     :text "source"
                     :start-line 1
                     :end-line 1
                     :point-line 1)))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (should e-chat-response-navigation-mode)
          (e-chat-add-context-reference-to-session
           reference
           e-chat-harness
           e-chat-session-id)
          (should-not e-chat-response-navigation-mode)
          (should-not e-chat-block-view-mode)
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-point-in-composer-p)))
          (should-not (eq (key-binding (kbd "j") t)
                          #'e-chat-response-navigation-next)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-inspect-error-targets-newest-failure-outside-block ()
  "e-inspect-error falls back to the newest persisted failed turn."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         prompt)
    (e-harness-create-session harness :id "older-session")
    (e-session-append-message
     store "older-session"
     '(:id "older-msg" :role user :content "older" :turn-id "older-turn"))
    (e-session-append-activity-event
     store "older-session" "older-turn" 'turn-failed
     '(:error "older failure"))
    (e-harness-create-session harness :id "newer-session")
    (e-session-append-message
     store "newer-session"
     '(:id "newer-msg" :role user :content "newer" :turn-id "newer-turn"))
    (e-session-append-activity-event
     store "newer-session" "newer-turn" 'turn-failed
     '(:error "newer failure"))
    (cl-letf (((symbol-function 'e-chat-create-session)
               (lambda (&rest _args) '(:id "inspection-session")))
              ((symbol-function 'e-chat-open-session)
               (lambda (&rest _args) nil))
              ((symbol-function 'e-chat-submit-session)
               (lambda (_harness _session-id submitted-prompt &rest _args)
                 (setq prompt submitted-prompt))))
      (e-inspect-error :harness harness)
      (should (string-match-p "newer-session" prompt))
      (should (string-match-p "newer-turn" prompt)))))

(ert-deftest e-chat-test-open-loaded-session-replay-remains-bounded ()
  "Loaded-session replay never backfills omitted transcript history."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-chat-session-replay-message-limit 2)
         buffer)
    (unwind-protect
        (progn
          (e-chat-test--create-session store :id "loaded-bounded"
                            :metadata '(:name "Loaded bounded"))
          (dolist (message
                   '((:id "msg-1" :role user :content "first prompt")
                     (:id "msg-2" :role assistant :content "first response")
                     (:id "msg-3" :role user :content "middle prompt")
                     (:id "msg-4" :role user :content "last prompt")
                     (:id "msg-5" :role assistant :content "last response")))
            (e-session-append-message store "loaded-bounded" message))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "loaded-bounded")
          (setq buffer (e-chat-open-session harness "loaded-bounded"))
          (with-current-buffer (e-chat-test--composer buffer)
            (goto-char (point-max))
            (insert "next")
            (should (equal (e-chat-composer-text) "next")))
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              (should (string-match-p
                       "3 earlier transcript messages omitted" text))
              (should-not (string-match-p "first prompt" text))))
          ;; Drain any deferred presentation work.  Omitted history must not
          ;; reappear after the initial paint has returned.
          (with-current-buffer buffer
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (let ((text (buffer-string)))
              (should (string-match-p
                       "3 earlier transcript messages omitted" text))
              (should-not (string-match-p "first prompt" text))
              (should-not (string-match-p "first response" text))
              (should-not (string-match-p "middle prompt" text))
              (should (string-match-p "last prompt" text))
              (should (string-match-p "last response" text))
              (should (equal (e-chat-test--composer-text-for buffer) "next")))
            (e-chat-transcript-rerender)
            (let ((text (buffer-string)))
              (should (string-match-p
                       "3 earlier transcript messages omitted" text))
              (should-not (string-match-p "first prompt" text))
              (should-not (string-match-p "middle prompt" text))
              (should (string-match-p "last response" text))
              (should (equal (e-chat-test--composer-text-for buffer) "next")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-transcript-composition-test)

;;; e-chat-transcript-composition-test.el ends here
