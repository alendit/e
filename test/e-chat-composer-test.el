;;; e-chat-composer-test.el --- Composer owner contract tests -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests exercise editing, submission extraction, and inline reference
;; ownership without loading the composed chat facade.

;;; Code:

(load (expand-file-name "e-chat-owner-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)
(require 'e-chat-surface)
(require 'e-chat-transcript)
(require 'e-chat-composer)

(defun e-chat-composer-test--buffer ()
  "Create and initialize one standalone composer buffer."
  (let ((buffer (e-chat-owner-test--buffer " *composer owner test*")))
    (with-current-buffer buffer
      (e-chat-composer-mode)
      (e-chat-transcript-reset)
      (e-chat-composer-initialize))
    buffer))

(ert-deftest e-chat-composer-owner-initializes-editable-boundary ()
  "Composer initialization exposes only editable text after its prompt."
  (let ((buffer (e-chat-composer-test--buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (insert "draft")
          (should (e-chat-composer-active-p))
          (should (equal (e-chat-composer-text) "draft"))
          (should (e-chat-composer--point-in-composer-p))
          (goto-char (point-min))
          (should-not (e-chat-composer--point-in-composer-p))
          (should (equal (plist-get (e-chat-composer-submission) :prompt)
                         "draft")))
      (e-chat-owner-test--kill-buffer buffer))))

(ert-deftest e-chat-composer-owner-keeps-inline-reference-atomic ()
  "Inline context references are protected atoms in the composer."
  (let ((buffer (e-chat-composer-test--buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (insert "before ")
          (let ((reference
                 (e-chat-composer-insert-context-reference
                  '(:uri "file:///tmp/source" :label "source" :text "body"))))
            (should (equal (plist-get reference :label) "source"))
            (insert " after")
            (should (string-match-p "@\\[source\\]"
                                    (e-chat-composer-text)))
            (should (e-chat-composer-delete-context-reference-at
                     (- (point) 6)))
            (should (equal (e-chat-composer-text) "before  after"))))
      (e-chat-owner-test--kill-buffer buffer))))

(ert-deftest e-chat-composer-owner-formats-reference-submission ()
  "Submission extraction appends model-facing reference metadata."
  (let* ((reference '(:id "ref-1" :uri "file:///tmp/source"
                      :label "source" :text "body"))
         (prompt (e-chat-format-reference-prompt "question"
                                                  (list reference)))
         (placeholder (e-chat-reference-placeholder reference)))
    (should (string-match-p "References:" prompt))
    (should (string-match-p "source" prompt))
    (should (equal placeholder "<reference id=\"ref-1\" label=\"source\">"))))

(ert-deftest e-chat-composer-owner-sanitizes-presentation-properties ()
  "Composer sanitization strips transcript-only text properties."
  (let ((text (copy-sequence "draft")))
    (add-text-properties 0 (length text)
                         '(read-only t e-chat-block-id "block-1")
                         text)
    (let ((clean (e-chat-composer--sanitize-composer-text text)))
      (should (equal clean "draft"))
      (should-not (get-text-property 0 'read-only clean))
      (should-not (get-text-property 0 'e-chat-block-id clean)))))
