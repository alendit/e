;;; e-structured-blocks-test.el --- Tests for the structured-block registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the core structured-block registry and render transform.

;;; Code:

(require 'ert)
(require 'e-structured-blocks)

(defun e-structured-blocks-test--fence-matcher (open close)
  "Return a matcher for a `OPEN' ... `CLOSE' fenced block."
  (lambda (content)
    (let (matches (start 0))
      (while (and (< start (length content))
                 (string-match (regexp-quote open) content start))
        (let* ((match-start (match-beginning 0))
               (close-pos (string-match (regexp-quote close) content
                                        (match-end 0))))
          (if close-pos
              (progn
                (push (list :start match-start
                            :end (+ close-pos (length close)))
                      matches)
                (setq start (+ close-pos (length close))))
            (setq start (length content)))))
      (nreverse matches))))

(ert-deftest e-structured-blocks-test-hidden-block-removed-and-extracted ()
  "A hidden kind is removed from display text and returned among blocks."
  (let ((registry (e-structured-blocks-registry-create)))
    (e-structured-blocks-register
     registry
     (e-structured-block-create
      :kind 'reasoning
      :matcher (e-structured-blocks-test--fence-matcher
                "#+begin_reasoning" "#+end_reasoning")
      :display 'hidden
      :parser (lambda (text) (list :raw text))))
    (let* ((content
            (concat "Before.\n"
                    "#+begin_reasoning\nconfidence: high\n#+end_reasoning\n"
                    "After."))
           (result (e-structured-blocks-render content registry))
           (text (plist-get result :text))
           (blocks (plist-get result :blocks)))
      (should (equal text "Before.\n\nAfter."))
      (should (= (length blocks) 1))
      (let ((block (car blocks)))
        (should (eq (plist-get block :kind) 'reasoning))
        (should (equal (plist-get block :text)
                       "#+begin_reasoning\nconfidence: high\n#+end_reasoning"))
        (should (equal (plist-get block :parsed)
                       (list :raw "#+begin_reasoning\nconfidence: high\n#+end_reasoning")))))))

(ert-deftest e-structured-blocks-test-unregistered-content-passes-through ()
  "Content with no registered kinds renders byte-for-byte unchanged."
  (let ((registry (e-structured-blocks-registry-create))
        (content "Plain assistant text with #+begin_reasoning inside it."))
    (let ((result (e-structured-blocks-render content registry)))
      (should (eq (plist-get result :text) content))
      (should-not (plist-get result :blocks)))
    ;; A nil registry (no attached session) behaves the same way.
    (let ((result (e-structured-blocks-render content nil)))
      (should (eq (plist-get result :text) content))
      (should-not (plist-get result :blocks)))))

(ert-deftest e-structured-blocks-test-inline-and-collapsed-are-inert ()
  "Non-hidden dispositions are declared but leave display text unchanged."
  (let ((registry (e-structured-blocks-registry-create)))
    (e-structured-blocks-register
     registry
     (e-structured-block-create
      :kind 'citation
      :matcher (e-structured-blocks-test--fence-matcher "[[" "]]")
      :display 'inline))
    (e-structured-blocks-register
     registry
     (e-structured-block-create
      :kind 'plan
      :matcher (e-structured-blocks-test--fence-matcher "<<" ">>")
      :display 'collapsed))
    (let* ((content "See [[source]] and <<plan here>>.")
           (result (e-structured-blocks-render content registry)))
      (should (equal (plist-get result :text) content))
      (should (= (length (plist-get result :blocks)) 2)))))

(ert-deftest e-structured-blocks-test-register-rejects-duplicate-kind ()
  "Registering the same kind twice in one registry is rejected."
  (let ((registry (e-structured-blocks-registry-create))
        (spec (lambda ()
                (e-structured-block-create
                 :kind 'reasoning
                 :matcher (lambda (_content) nil)
                 :display 'hidden))))
    (e-structured-blocks-register registry (funcall spec))
    (should-error (e-structured-blocks-register registry (funcall spec))
                 :type 'e-structured-blocks-duplicate-kind)))

(ert-deftest e-structured-blocks-test-unregister-and-reset ()
  "Unregister removes one kind; reset clears the whole registry."
  (let ((registry (e-structured-blocks-registry-create)))
    (e-structured-blocks-register
     registry
     (e-structured-block-create
      :kind 'reasoning
      :matcher (lambda (_content) nil)
      :display 'hidden))
    (should (e-structured-blocks-for-kind registry 'reasoning))
    (e-structured-blocks-unregister registry 'reasoning)
    (should-not (e-structured-blocks-for-kind registry 'reasoning))
    (e-structured-blocks-register
     registry
     (e-structured-block-create
      :kind 'plan
      :matcher (lambda (_content) nil)
      :display 'collapsed))
    (e-structured-blocks-registry-reset registry)
    (should-not (e-structured-blocks-list registry))))

(ert-deftest e-structured-blocks-test-core-module-has-no-shell-or-capability-dependency ()
  "The registry source declares no shell or capability `require'.
This is a static dependency-direction check on the module's own top-level
requires, independent of what else happens to be loaded in the same Emacs
process by other tests."
  (should (featurep 'e-structured-blocks))
  (let ((forbidden '("e-chat" "e-modernchat" "e-capabilities" "e-harness"))
        (requires nil))
    (with-temp-buffer
      (insert-file-contents (locate-library "e-structured-blocks.el"))
      (goto-char (point-min))
      (while (re-search-forward "(require '\([a-zA-Z0-9-]+\)" nil t)
        (push (match-string 1) requires)))
    (dolist (name forbidden)
      (should-not (member name requires)))))

(provide 'e-structured-blocks-test)

;;; e-structured-blocks-test.el ends here
