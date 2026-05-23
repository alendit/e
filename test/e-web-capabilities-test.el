;;; e-web-capabilities-test.el --- Tests for web capabilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the basic web layer capability surface.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-resources)
(require 'e-tools)
(require 'e-web)

(ert-deftest e-web-capabilities-test-layer-skeleton ()
  "The web layer exposes compact guidance, tools, and reference resources."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (layer (e-web-layer-create)))
    (should (eq (e-layer-id layer) 'web))
    (should (equal (mapcar #'e-capability-id
                           (e-layer-capabilities layer))
                   '(web)))
    (e-harness-activate-layer harness layer)
    (let ((tool-names (mapcar (lambda (definition)
                                (plist-get definition :name))
                              (e-tools-definitions (e-harness-tools harness)))))
      (should (equal tool-names
                     '("read"
                       "web_search"
                       "web_fetch"
                       "web_browser"))))
    (let ((instructions
           (e-capability-instructions
            (car (e-layer-capabilities layer)))))
      (should (string-match-p "Web access is available" instructions))
      (should (string-match-p "e://web/refs/overview.md" instructions))
      (should (< (length instructions) 260)))
    (dolist (uri '("e://web/refs/overview.md"
                   "e://web/refs/search.md"
                   "e://web/refs/fetch.md"
                   "e://web/refs/browser.md"
                   "e://web/refs/boundaries.md"))
      (should (string-match-p
               "web"
               (e-resources-read (e-harness-resources harness) uri nil))))
    (let ((browser-reference
           (e-resources-read
            (e-harness-resources harness)
            "e://web/refs/browser.md"
            nil)))
      (should (string-match-p "`web_browser`" browser-reference))
      (should (string-match-p "`operation`" browser-reference))
      (should (string-match-p "`click`" browser-reference)))))

(provide 'e-web-capabilities-test)

;;; e-web-capabilities-test.el ends here
