;;; e-chat-presentation-integration-test.el --- Chat facade smoke tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Small public composed-chat smoke scenarios.  Semantic surface, composer,
;; transcript, activity, and overview groups live in independently runnable
;; integration suites.

;;; Code:

(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-chat-test-composed-surface-keeps-draft-outside-transcript ()
  "Transcript rendering must not recreate or alter the separate composer."
  (let* ((buffer (e-chat-test--buffer nil "chat-composed-surface")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((composer (e-chat-surface-composer-buffer))
                (draft "keep this unsent draft")
                composer-tick)
            (should (buffer-live-p composer))
            (should (eq (e-chat-surface-transcript-buffer composer)
                        buffer))
            (with-current-buffer composer
              (goto-char (point-max))
              (insert draft)
              (setq composer-tick (buffer-chars-modified-tick)))
            (e-chat-render-event
             (e-events-make :type 'message-added
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:message (:role user
                                                  :content "render this"))))
            (should-not (string-match-p (regexp-quote (e-chat-composer-glyph))
                                        (buffer-string)))
            (should (string-match-p "render this" (buffer-string)))
            (with-current-buffer composer
              (should (equal (e-chat-composer-text) draft))
              (should (= (buffer-chars-modified-tick) composer-tick)))
            (e-chat-composer-insert-context-reference
             '(:uri "file:///tmp/context" :label "context" :text "source"))
            (with-current-buffer composer
              (should (string-match-p "@\\[context\\]"
                                      (e-chat-composer-text))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-open-creates-protected-transcript-and-composer ()
  "Opening chat creates protected transcript text and editable composer text."
  (let ((buffer (e-chat-test--buffer nil "chat-open")))
    (unwind-protect
        (with-current-buffer buffer
          (should (derived-mode-p 'e-chat-mode))
          (goto-char (point-min))
          (should (looking-at-p (regexp-quote "E Agent Session")))
          (should (eq (get-text-property (point-min) 'font-lock-face)
                      'e-chat-title-face))
          (should-not (string-match-p "^e chat$" (buffer-string)))
          (should (get-text-property (point-min) 'read-only))
          (goto-char (point-min))
          (should-error (insert "mutate") :type 'buffer-read-only)
          (should-not (string-match-p (regexp-quote (e-chat-composer-glyph))
                                      (buffer-string)))
          (with-current-buffer (e-chat-test--composer buffer)
            (should (derived-mode-p 'e-chat-composer-mode))
            (should (number-or-marker-p (e-chat-composer-start-position)))
            (goto-char (point-max))
            (insert "editable")
            (should (equal (e-chat-composer-text) "editable"))
            (should (string-match-p (regexp-quote (e-chat-composer-glyph))
                                    (buffer-string)))
            (should (get-text-property
                     (1- (e-chat-composer-start-position))
                     'e-chat-composer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-submit-multiline-composer-and-render-response ()
  "The composer submits multiline text and chat renders message blocks."
  (let ((buffer (e-chat-test--buffer
                 '((:type assistant-message :content "hello back")
                   (:type done :reason stop))
                 "chat-submit")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "first line\nsecond line")
          (e-chat-submit)
          (should (e-chat-test--wait-until
                   (lambda () (string-match-p "hello back" (with-current-buffer buffer (buffer-string))))
                   1.0))
          (let ((content (with-current-buffer buffer (buffer-string))))
            (should (string-match-p (concat (regexp-quote (e-chat-transcript-user-glyph))
                                            " first line\nsecond line")
                                    content))
            (should (string-match-p
                     (concat (regexp-quote (e-chat-transcript-assistant-glyph))
                             " hello back")
                     content))
            (should-not (string-match-p "Turn started" content))
            (should-not (string-match-p "Turn finished" content))
            (should-not (string-match-p "Backend returned no assistant output"
                                        content)))
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-min))
              (search-forward (concat (e-chat-transcript-user-glyph) " first line"))
              (should (eq (get-text-property (point) 'font-lock-face)
                          'e-chat-user-face))
              (search-forward "hello back")
              (should-not (eq (get-text-property (point) 'font-lock-face)
                              'e-chat-assistant-face))))
          (should (equal (e-chat-composer-text) "")))
      (when (buffer-live-p buffer)
        (with-current-buffer (e-chat-test--composer buffer)
          (ignore-errors
            (e-harness-test-abort e-chat-harness e-chat-session-id)))
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-overview-mode-neutralizes-evil ()
  "Overview mode keeps Evil from intercepting sidebar navigation keys."
  (let ((buffer (get-buffer-create "*e-chat-overview-evil-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'evil-local-mode)
                   (lambda (argument)
                     (setq-local evil-local-mode
                                 (not (and (numberp argument)
                                           (< argument 0))))
                     (unless evil-local-mode
                       (setq-local evil-state nil)))))
          (with-current-buffer buffer
            (setq-local evil-local-mode t)
            (setq-local evil-state 'normal)
            (e-chat-overview-mode)
            (should-not evil-local-mode)
            (should-not evil-state)
            (should (eq (lookup-key e-chat-overview-mode-map (kbd "RET"))
                        #'e-chat-overview-open-session))
            (should (eq (lookup-key e-chat-overview-mode-map (kbd "j"))
                        #'e-chat-overview-next-session))
            (should (eq (lookup-key e-chat-overview-mode-map (kbd "k"))
                        #'e-chat-overview-previous-session))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-presentation-integration-test)

;;; e-chat-presentation-integration-test.el ends here
