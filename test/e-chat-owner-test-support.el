;;; e-chat-owner-test-support.el --- Minimal presentation-owner fixtures -*- lexical-binding: t; -*-

;;; Commentary:

;; This support file intentionally has no dependency on `e-chat'.  Owner tests
;; construct only the buffer-local state needed by the owner contract under
;; test; composed lifecycle and window behavior belongs to
;; `e-chat-presentation-integration-test'.

;;; Code:

(require 'ert)

;; The presentation owners intentionally do not provide a facade's session
;; binding.  Give standalone fixtures the same unbound-safe core variables a
;; composed chat buffer receives from `e-chat-open'.
(defvar e-current-harness nil)
(defvar e-chat-harness nil)
(defvar e-chat-session-id nil)
(defvar e-chat-harness-instance-id nil)

(defun e-chat-owner-test--buffer (&optional name)
  "Create an undisplayed scratch buffer for an owner contract test."
  (generate-new-buffer (or name " *e-chat owner test*")))

(defun e-chat-owner-test--kill-buffer (buffer)
  "Kill BUFFER when it is live."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(provide 'e-chat-owner-test-support)

;;; e-chat-owner-test-support.el ends here
