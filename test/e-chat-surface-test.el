;;; e-chat-surface-test.el --- Surface owner contract tests -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests load only the surface owner and its stable core dependencies.
;; Composed transcript/composer windows are covered by the facade integration
;; suite.

;;; Code:

(load (expand-file-name "e-chat-owner-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)
(require 'e-chat-surface)

(ert-deftest e-chat-surface-owner-marks-transcript-and-composer ()
  "Surface membership is explicit and pairing is owned by the surface."
  (let ((transcript (e-chat-owner-test--buffer " *surface transcript*"))
        (composer (e-chat-owner-test--buffer " *surface composer*")))
    (unwind-protect
        (progn
          (e-chat-surface-mark-transcript transcript)
          (e-chat-surface-mark-composer composer)
          (e-chat-surface-bind-composer composer transcript)
          (should (e-chat-surface-transcript-p transcript))
          (should (e-chat-surface-composer-p composer))
          (should (eq (e-chat-surface-transcript-buffer composer)
                      transcript))
          (should (eq (e-chat-surface-composer-buffer transcript)
                      composer)))
      (e-chat-owner-test--kill-buffer composer)
      (e-chat-owner-test--kill-buffer transcript))))

(ert-deftest e-chat-surface-owner-status-and-redraw-ports-are-local ()
  "Surface status and redraw visibility stay buffer-local."
  (let ((first (e-chat-owner-test--buffer " *surface first*"))
        (second (e-chat-owner-test--buffer " *surface second*")))
    (unwind-protect
        (progn
          (e-chat-surface-mark-transcript first)
          (e-chat-surface-mark-transcript second)
          (with-current-buffer first
            (e-chat-surface-set-status "running")
            (e-chat-surface-set-redraw-visible t))
          (should (equal (e-chat-surface-status first) "running"))
          (should (e-chat-surface-redraw-visible-p first))
          (should-not (e-chat-surface-status second))
          (should-not (e-chat-surface-redraw-visible-p second)))
      (e-chat-owner-test--kill-buffer second)
      (e-chat-owner-test--kill-buffer first))))

(ert-deftest e-chat-surface-owner-notifies-only-status-changes ()
  "Transient status listeners receive changes without duplicate refreshes."
  (let ((transcript (e-chat-owner-test--buffer " *surface status hook*"))
        changes)
    (unwind-protect
        (with-current-buffer transcript
          (e-chat-surface-mark-transcript transcript)
          (add-hook 'e-chat-surface-status-changed-hook
                    (lambda (status) (push status changes)) nil t)
          (e-chat-surface-set-status "streaming")
          (e-chat-surface-set-status "streaming" t)
          (e-chat-surface-set-status "done")
          (should (equal changes '("done" "streaming"))))
      (e-chat-owner-test--kill-buffer transcript))))

(ert-deftest e-chat-surface-owner-unbind-clears-both-ends ()
  "Unbinding a surface removes only its ephemeral pairing state."
  (let ((transcript (e-chat-owner-test--buffer " *surface unbind transcript*"))
        (composer (e-chat-owner-test--buffer " *surface unbind composer*")))
    (unwind-protect
        (progn
          (e-chat-surface-mark-transcript transcript)
          (e-chat-surface-mark-composer composer)
          (e-chat-surface-bind-composer composer transcript)
          (e-chat-surface-unbind-composer composer transcript)
          (should-not (e-chat-surface-composer-buffer transcript))
          (should-not (e-chat-surface-transcript-buffer composer))
          (should-not (e-chat-surface-composer-p composer)))
      (e-chat-owner-test--kill-buffer composer)
      (e-chat-owner-test--kill-buffer transcript))))

(ert-deftest e-chat-surface-owner-display-text-is-host-neutral ()
  "The surface exposes semantic status text independently of windows."
  (should (equal (e-chat-surface-mode-line-display-text "streaming")
                 "streaming"))
  (should (equal (e-chat-surface-mode-line-display-text "e-chat") "e-chat")))

(provide 'e-chat-surface-test)

;;; e-chat-surface-test.el ends here
