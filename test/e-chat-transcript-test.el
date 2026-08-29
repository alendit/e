;;; e-chat-transcript-test.el --- Transcript owner contract tests -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests load the transcript owner and its surface/core collaborators.
;; Facade event composition and window behavior live in the integration suite.

;;; Code:

(load (expand-file-name "e-chat-owner-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)
(require 'e-chat-surface)
(require 'e-chat-transcript)

(defun e-chat-transcript-test--buffer ()
  "Create a reset transcript-owner buffer."
  (let ((buffer (e-chat-owner-test--buffer " *transcript owner test*")))
    (with-current-buffer buffer
      (e-chat-surface-mark-transcript)
      (e-chat-transcript-reset))
    buffer))

(ert-deftest e-chat-transcript-owner-inserts-protected-entry-record ()
  "Transcript insertion records a navigable, protected durable entry."
  (let ((buffer (e-chat-transcript-test--buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-transcript-insert-entry "You" "hello" nil "turn-1")
          (goto-char (point-min))
          (search-forward "hello")
          (let ((block-id (e-chat-transcript-block-at-point)))
            (should block-id)
            (should (equal (plist-get (e-chat-transcript-focused-block) :turn-id)
                           "turn-1"))
            (should (get-text-property (point) 'read-only))
            (should (equal (e-chat-transcript-focused-turn-id) "turn-1"))))
      (e-chat-owner-test--kill-buffer buffer))))

(ert-deftest e-chat-transcript-owner-reset-rebuilds-ephemeral-registries ()
  "Reset drops display-local block and turn projections."
  (let ((buffer (e-chat-transcript-test--buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-transcript-insert-entry "You" "old" nil "turn-old")
          (should (e-chat-transcript-turn-id-at-point))
          (e-chat-transcript-reset)
          (should-not (gethash "turn-old" e-chat-transcript--turn-registry))
          (should-not e-chat-transcript--block-order)
          (should (hash-table-p e-chat-transcript--block-registry)))
      (e-chat-owner-test--kill-buffer buffer))))

(ert-deftest e-chat-transcript-owner-keeps-participant-identity-pure ()
  "Board-shaped events are isolated while ordinary events remain selected."
  (should (e-chat-transcript-event-selected-participant-p '(:type message)))
  (should-not
   (e-chat-transcript-event-selected-participant-p
    '(:type message :board-id "board-1" :board-seq 2)))
  (should (equal
           (e-chat-transcript-observed-turn-id "turn-1"
                                               '(:board-id "b" :board-seq 2))
           "turn-1:observed:2")))

(ert-deftest e-chat-transcript-owner-bounds-replay-and-summary-views ()
  "Transcript metadata helpers apply their explicit display bounds."
  (let ((e-chat-session-summary-preview-max-chars 5)
        (e-chat-session-replay-message-limit 2))
    (should (equal (e-chat-transcript-session-summary-preview
                   '(:summary "abcdef"))
                   "abcde…"))
    (should (= (e-chat-transcript-session-replay-message-count
                '(a b c))
               2))
    (should (equal (e-chat-transcript--tail-messages '(a b c) 2)
                   '(b c)))
    (should-error (e-chat-transcript-validated-replay-limit 0 'limit)
                  :type 'user-error)))

(ert-deftest e-chat-transcript-owner-exports-navigation-contract ()
  "Navigation modes and keymaps are defined by the transcript owner."
  (should (fboundp #'e-chat-response-navigation-mode))
  (should (fboundp #'e-chat-block-view-mode))
  (should (fboundp #'e-chat-tool-list-mode))
  (should (keymapp e-chat-response-navigation-mode-map))
  (should (commandp #'e-chat-response-navigation-next))
  (should (commandp #'e-chat-block-view-back)))
