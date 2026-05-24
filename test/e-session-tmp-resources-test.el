;;; e-session-tmp-resources-test.el --- Tests for tmp:// session resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for session-scoped temporary resources.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-resources)

(ert-deftest e-session-tmp-test-helper-writes-and-resource-read-recovers ()
  "The helper writes session tmp content and returns a readable tmp:// URI."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers
                   (list (e-layer-create
                          :id 'tmp-layer
                          :name "Tmp Layer"
                          :capabilities
                          (list (e-session-tmp-capability-create))))))
         (uri (e-session-tmp-write
               harness
               "session-1"
               "tool-results/turn-1/call-1.txt"
               "full output")))
    (should (equal uri "tmp://tool-results/turn-1/call-1.txt"))
    (should (equal (e-resources-read
                    (e-harness-resources harness "session-1" "turn-1")
                    uri
                    nil)
                   "full output"))))

(ert-deftest e-session-tmp-test-resource-write-creates-parents ()
  "Model-facing tmp:// writes create missing parents inside the session root."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers
                   (list (e-layer-create
                          :id 'tmp-layer
                          :name "Tmp Layer"
                          :capabilities
                          (list (e-session-tmp-capability-create))))))
         (resources (e-harness-resources harness "session-1" "turn-1")))
    (should (equal (e-resources-write resources
                                      "tmp://notes/new.txt"
                                      "created")
                   "tmp://notes/new.txt"))
    (should (equal (e-resources-read resources "tmp://notes/new.txt" nil)
                   "created"))))

(ert-deftest e-session-tmp-test-resource-edit-is-strict ()
  "tmp:// edits apply exact replacements to existing files only."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers
                   (list (e-layer-create
                          :id 'tmp-layer
                          :name "Tmp Layer"
                          :capabilities
                          (list (e-session-tmp-capability-create))))))
         (resources (e-harness-resources harness "session-1" "turn-1")))
    (e-resources-write resources "tmp://notes/edit.txt" "alpha beta alpha")
    (should (equal (e-resources-edit
                    resources
                    "tmp://notes/edit.txt"
                    '((:oldText "beta" :newText "BETA")))
                   "tmp://notes/edit.txt"))
    (should (equal (e-resources-read resources "tmp://notes/edit.txt" nil)
                   "alpha BETA alpha"))
    (should-error (e-resources-edit
                   resources
                   "tmp://notes/edit.txt"
                   '((:oldText "missing" :newText "x")))
                  :type 'e-session-tmp-resources-edit-mismatch)
    (should-error (e-resources-edit
                   resources
                   "tmp://notes/missing.txt"
                   '((:oldText "x" :newText "y")))
                  :type 'file-missing)))

(ert-deftest e-session-tmp-test-rejects-unsafe-paths ()
  "tmp:// resource paths stay inside the session root."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers
                   (list (e-layer-create
                          :id 'tmp-layer
                          :name "Tmp Layer"
                          :capabilities
                          (list (e-session-tmp-capability-create))))))
         (resources (e-harness-resources harness "session-1" "turn-1")))
    (should-error (e-session-tmp-write harness "session-1" "../escape.txt" "x")
                  :type 'e-session-tmp-resources-invalid-path)
    (should-error (e-resources-read resources "tmp://../escape.txt" nil)
                  :type 'e-session-tmp-resources-invalid-path)
    (should-error (e-resources-write resources "tmp:///absolute.txt" "x")
                  :type 'e-session-tmp-resources-invalid-path)
    (should-error (e-resources-write resources "tmp://bad\0name.txt" "x")
                  :type 'e-session-tmp-resources-invalid-path)))

(ert-deftest e-session-tmp-test-read-supports-line-ranges ()
  "tmp:// reads support the first-slice line range contract."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers
                   (list (e-layer-create
                          :id 'tmp-layer
                          :name "Tmp Layer"
                          :capabilities
                          (list (e-session-tmp-capability-create))))))
         (resources (e-harness-resources harness "session-1" "turn-1")))
    (e-resources-write resources "tmp://notes/range.txt" "one\ntwo\nthree\n")
    (should (equal (e-resources-read
                    resources
                    "tmp://notes/range.txt"
                    '(:unit "line" :start 2 :end 3))
                   "two\nthree\n"))))

(provide 'e-session-tmp-resources-test)

;;; e-session-tmp-resources-test.el ends here
