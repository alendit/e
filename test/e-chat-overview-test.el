;;; e-chat-overview-test.el --- Overview owner contract tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Overview catalog, row labeling, and command-map tests do not load the chat
;; facade.  Session opening and resume preview composition are integration
;; behavior covered by `e-chat-overview-integration-test'.

;;; Code:

(load (expand-file-name "e-chat-owner-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)
(require 'e-chat-overview)

(ert-deftest e-chat-overview-owner-identifies-board-sessions ()
  "Only board-native session metadata is eligible for the overview catalog."
  (should (e-chat-overview-board-session-p
           '(:id "board" :board-id "board-1")))
  (should (e-chat-overview-board-session-p
           '(:id "nested" :board-session-state (:board-id "board-2"))))
  (should-not (e-chat-overview-board-session-p '(:id "plain"))))

(ert-deftest e-chat-overview-owner-labels-session-candidates ()
  "Overview labels retain title/id and optionally the owning instance."
  (let* ((session '(:id "session-123456789" :title "Demo"))
         (candidate (list :session session)))
    (should (equal (e-chat-overview-session-choice-label session)
                   "Demo  [session-123456789]"))
    (should (equal (e-chat-overview-session-candidate-label candidate)
                   "Demo  [session-123456789]"))
    (should (equal (e-chat-overview-short-session-id "0123456789abcdef")
                   "0123456789ab"))))

(ert-deftest e-chat-overview-owner-mode-map-is-owned-here ()
  "Overview mode owns its navigation commands and keymap."
  (let ((map (e-chat-overview--make-mode-map)))
    (should (fboundp #'e-chat-overview-mode))
    (should (keymapp map))
    (should (eq (lookup-key map (kbd "RET"))
                #'e-chat-overview-select-session))
    (should (eq (lookup-key map (kbd "j"))
                #'e-chat-overview-next-session))
    (should (eq (lookup-key map (kbd "v"))
                #'e-chat-overview-preview-session))))

(ert-deftest e-chat-overview-owner-preview-contract-is-explicit ()
  "The overview exposes a stable reusable preview buffer contract."
  (should (equal (e-chat-overview-resume-preview-buffer-name)
                 "*e-chat-resume-preview*"))
  (should (commandp #'e-chat-overview-preview-session))
  (should (commandp #'e-chat-sidebar-toggle)))
