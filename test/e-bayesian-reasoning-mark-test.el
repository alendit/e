;;; e-bayesian-reasoning-mark-test.el --- Tests for the inline reasoning mark -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the Slice 2b inline reasoning mark: its deterministic
;; parser, matcher, and registration with the core structured-block registry.

;;; Code:

(require 'ert)
(require 'e-bayesian-reasoning)
(require 'e-structured-blocks)

(defconst e-bayesian-reasoning-mark-test--org-block
  "#+begin_reasoning
claim: the outage was caused by a config rollback
confidence: medium
alternatives: a network partition
evidence: incident-1234
#+end_reasoning")

(defconst e-bayesian-reasoning-mark-test--markdown-block
  "```reasoning
claim: the outage was caused by a config rollback
confidence: medium
alternatives: a network partition
evidence: incident-1234
```")

(defconst e-bayesian-reasoning-mark-test--expected-fields
  (list :claim "the outage was caused by a config rollback"
        :confidence "medium"
        :alternatives "a network partition"
        :evidence "incident-1234"))

(ert-deftest e-bayesian-reasoning-mark-test-parses-well-formed-org-block ()
  "A well-formed org-fence block parses to the expected field plist."
  (should (equal (e-bayesian-reasoning-parse-reasoning-block
                  e-bayesian-reasoning-mark-test--org-block)
                 e-bayesian-reasoning-mark-test--expected-fields)))

(ert-deftest e-bayesian-reasoning-mark-test-parses-well-formed-markdown-block ()
  "A well-formed markdown-fence block parses to the expected field plist."
  (should (equal (e-bayesian-reasoning-parse-reasoning-block
                  e-bayesian-reasoning-mark-test--markdown-block)
                 e-bayesian-reasoning-mark-test--expected-fields)))

(ert-deftest e-bayesian-reasoning-mark-test-insufficient-evidence-alternative ()
  "The literal `insufficient-evidence' alternative parses like any other value."
  (let ((block "#+begin_reasoning
claim: whether the customer upgraded last quarter
confidence: low
alternatives: insufficient-evidence
evidence:
#+end_reasoning"))
    (should (equal (e-bayesian-reasoning-parse-reasoning-block block)
                   (list :claim "whether the customer upgraded last quarter"
                         :confidence "low"
                         :alternatives "insufficient-evidence"
                         :evidence "")))))

(ert-deftest e-bayesian-reasoning-mark-test-malformed-block-is-nil ()
  "A block missing its closing fence, or with no field lines, parses to nil."
  (should-not (e-bayesian-reasoning-parse-reasoning-block
               "#+begin_reasoning\nclaim: x\n"))
  (should-not (e-bayesian-reasoning-parse-reasoning-block
               "#+begin_reasoning\nnot a field line at all\n#+end_reasoning")))

(ert-deftest e-bayesian-reasoning-mark-test-absent-block-is-nil ()
  "A string with no fence at all parses to nil."
  (should-not (e-bayesian-reasoning-parse-reasoning-block "just plain prose"))
  (should-not (e-bayesian-reasoning-parse-reasoning-block "")))

(ert-deftest e-bayesian-reasoning-mark-test-matcher-finds-both-fence-forms ()
  "The matcher finds an org fence and a markdown fence, sorted by start."
  (let* ((content (concat "Before.\n" e-bayesian-reasoning-mark-test--org-block
                          "\nMiddle.\n" e-bayesian-reasoning-mark-test--markdown-block
                          "\nAfter."))
         (matches (e-bayesian-reasoning--reasoning-matcher content)))
    (should (= (length matches) 2))
    (should (< (plist-get (nth 0 matches) :start) (plist-get (nth 1 matches) :start)))
    (should (equal (substring content
                              (plist-get (nth 0 matches) :start)
                              (plist-get (nth 0 matches) :end))
                   e-bayesian-reasoning-mark-test--org-block))
    (should (equal (substring content
                              (plist-get (nth 1 matches) :start)
                              (plist-get (nth 1 matches) :end))
                   e-bayesian-reasoning-mark-test--markdown-block))))

(defun e-bayesian-reasoning-mark-test--registry ()
  "Return a registry with the capability's `reasoning' spec registered."
  (let ((registry (e-structured-blocks-registry-create))
        (capability (e-bayesian-reasoning-capability-create)))
    (dolist (spec (e-capability-structured-blocks capability))
      (e-structured-blocks-register registry spec))
    registry))

(ert-deftest e-bayesian-reasoning-mark-test-hidden-via-registry-org-mode ()
  "The org-fence block is hidden by the generic registry transform."
  (let* ((registry (e-bayesian-reasoning-mark-test--registry))
         (content (concat "Visible answer.\n"
                          e-bayesian-reasoning-mark-test--org-block
                          "\nMore visible text."))
         (result (e-structured-blocks-render content registry)))
    (should (equal (plist-get result :text)
                   "Visible answer.\n\nMore visible text."))
    (should (= (length (plist-get result :blocks)) 1))
    (should (equal (plist-get (car (plist-get result :blocks)) :parsed)
                   e-bayesian-reasoning-mark-test--expected-fields))
    ;; Non-mutating: the input string itself is untouched.
    (should (equal content (concat "Visible answer.\n"
                                   e-bayesian-reasoning-mark-test--org-block
                                   "\nMore visible text.")))))

(ert-deftest e-bayesian-reasoning-mark-test-hidden-via-registry-markdown-mode ()
  "The markdown-fence block is hidden by the generic registry transform."
  (let* ((registry (e-bayesian-reasoning-mark-test--registry))
         (content (concat "Visible answer.\n"
                          e-bayesian-reasoning-mark-test--markdown-block
                          "\nMore visible text."))
         (result (e-structured-blocks-render content registry)))
    (should (equal (plist-get result :text)
                   "Visible answer.\n\nMore visible text."))
    (should (= (length (plist-get result :blocks)) 1))
    (should (equal (plist-get (car (plist-get result :blocks)) :parsed)
                   e-bayesian-reasoning-mark-test--expected-fields))
    (should (equal content (concat "Visible answer.\n"
                                   e-bayesian-reasoning-mark-test--markdown-block
                                   "\nMore visible text.")))))

(ert-deftest e-bayesian-reasoning-mark-test-capability-registers-reasoning-kind ()
  "The capability's `:structured-blocks' slot registers exactly `reasoning'."
  (let ((capability (e-bayesian-reasoning-capability-create)))
    (should (= (length (e-capability-structured-blocks capability)) 1))
    (let ((spec (car (e-capability-structured-blocks capability))))
      (should (eq (e-structured-block-kind spec) 'reasoning))
      (should (eq (e-structured-block-display spec) 'hidden))
      (should (eq (e-structured-block-matcher spec)
                  #'e-bayesian-reasoning--reasoning-matcher))
      (should (eq (e-structured-block-parser spec)
                  #'e-bayesian-reasoning-parse-reasoning-block)))))

(provide 'e-bayesian-reasoning-mark-test)

;;; e-bayesian-reasoning-mark-test.el ends here
