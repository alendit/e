;;; e-output-style-test.el --- Tests for output style capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the output-style guidance capability: the style registry and
;; resolver, the capability constructor and its config option, the set/describe
;; helpers, and the end-to-end assertion that style prose reaches the system
;; channel rather than a conversation turn.

;;; Code:

(require 'ert)
(require 'seq)
(require 'e)
(require 'e-capabilities)
(require 'e-capability-config)
(require 'e-output-style)

;;;; Step 1: registry and resolver

(ert-deftest e-output-style-test-resolve-known-id ()
  "Resolving a known style returns its instructions string."
  (let ((instructions (e-output-style--resolve 'hemingway)))
    (should (stringp instructions))
    (should (string-match-p "Hemingway" instructions))))

(ert-deftest e-output-style-test-resolve-unknown-id-signals ()
  "Resolving an unknown style signals an error naming it and the known ids."
  (let ((err (should-error (e-output-style--resolve 'nonesuch))))
    (should (string-match-p "nonesuch" (error-message-string err)))
    (should (string-match-p "hemingway" (error-message-string err)))))

(ert-deftest e-output-style-test-register-adds-style ()
  "Registering a style makes it resolvable; it is removed on cleanup."
  (let ((e-output-style-registry (copy-tree e-output-style-registry)))
    (e-output-style-register 'pirate
                             :name "Pirate"
                             :instructions "Talk like a pirate, matey.")
    (should (equal (e-output-style--resolve 'pirate)
                   "Talk like a pirate, matey."))
    (should (memq 'pirate (e-output-style-ids)))))

(ert-deftest e-output-style-test-builtin-styles-present ()
  "The v1 built-in styles are registered with non-empty instructions."
  (dolist (id '(concise explanatory hemingway))
    (let ((instructions (e-output-style--resolve id)))
      (should (stringp instructions))
      (should-not (string-empty-p instructions)))))

;;;; Step 2: capability constructor

(ert-deftest e-output-style-test-capability-inert-by-default ()
  "With no style configured the capability contributes no instructions."
  (let ((e-capability-config nil)
        (directory (make-temp-file "e-output-style-" t)))
    (unwind-protect
        (let ((capability (e-output-style-capability-create directory)))
          (should (eq (e-capability-id capability) 'output-style))
          (should (null (e-capability-instructions capability))))
      (delete-directory directory t))))

(ert-deftest e-output-style-test-capability-uses-configured-style ()
  "A configured style id resolves to its prose as the capability instructions."
  (let ((e-capability-config '((output-style :style hemingway)))
        (directory (make-temp-file "e-output-style-" t)))
    (unwind-protect
        (let ((capability (e-output-style-capability-create directory)))
          (should (equal (e-capability-instructions capability)
                         (e-output-style--resolve 'hemingway)))
          (should (= (e-capability-instruction-priority capability) 260)))
      (delete-directory directory t))))

(ert-deftest e-output-style-test-capability-rejects-unknown-option ()
  "An unknown config option key for output-style signals the feature-07 error."
  (let ((e-capability-config '((output-style :voice loud)))
        (directory (make-temp-file "e-output-style-" t)))
    (unwind-protect
        (should-error
         (e-output-style-capability-create directory)
         :type 'e-capability-config-unknown-option)
      (delete-directory directory t))))

;;;; Step 4: set/describe helpers

(ert-deftest e-output-style-test-set-config-writes-style ()
  "The set helper writes the style under output-style without touching others."
  (let ((e-capability-config '((other-cap :flag t))))
    (e-output-style--set-config 'concise)
    (should (equal (cdr (assq 'output-style e-capability-config))
                   '(:style concise)))
    (should (equal (cdr (assq 'other-cap e-capability-config))
                   '(:flag t)))))

(ert-deftest e-output-style-test-set-config-rejects-unknown ()
  "Setting an unknown style id signals the resolver error."
  (let ((e-capability-config nil))
    (should-error (e-output-style--set-config 'bogus))))

(ert-deftest e-output-style-test-describe-string-active ()
  "The describe helper names the active style and includes its instructions."
  (let ((e-capability-config '((output-style :style hemingway)))
        (directory (make-temp-file "e-output-style-" t)))
    (unwind-protect
        (let ((text (e-output-style--describe-string directory)))
          (should (string-match-p "hemingway" text))
          (should (string-match-p "plain declarative" text)))
      (delete-directory directory t))))

(ert-deftest e-output-style-test-describe-string-inactive ()
  "With no style configured the describe helper reports the default voice."
  (let ((e-capability-config nil)
        (directory (make-temp-file "e-output-style-" t)))
    (unwind-protect
        (should (string-match-p "default voice"
                                (e-output-style--describe-string directory)))
      (delete-directory directory t))))

;;;; Step 5: end-to-end system-channel assertion

(ert-deftest e-output-style-test-style-prose-is-system-fragment ()
  "Configured style prose reaches the system channel, not a conversation turn."
  (let ((e-capability-config '((output-style :style hemingway)))
        (directory (make-temp-file "e-output-style-" t)))
    (unwind-protect
        (let* ((capability (e-output-style-capability-create directory))
               (messages (e-capabilities-context-messages (list capability)))
               (style (e-output-style--resolve 'hemingway))
               (system (seq-find
                        (lambda (message) (eq (plist-get message :role) 'system))
                        messages)))
          (should system)
          (should (equal (plist-get system :content) style))
          (should-not
           (seq-find
            (lambda (message)
              (and (not (eq (plist-get message :role) 'system))
                   (equal (plist-get message :content) style)))
            messages)))
      (delete-directory directory t))))

(provide 'e-output-style-test)

;;; e-output-style-test.el ends here
