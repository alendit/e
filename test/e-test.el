;;; e-test.el --- Tests for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the minimal public package surface.

;;; Code:

(require 'ert)

(ert-deftest e-test-loads-feature ()
  "The package feature can be required from the project load path."
  (should (require 'e nil t)))

(ert-deftest e-test-exposes-version ()
  "The package exposes its scaffold version."
  (require 'e)
  (should (string= e-version "0.1.0")))

(ert-deftest e-test-exposes-interactive-entrypoints ()
  "The package exposes status and live-reload commands."
  (require 'e)
  (should (commandp 'e-status))
  (should (commandp 'e-dev-reload)))

(ert-deftest e-test-exposes-core-harness-api ()
  "The package exposes the core harness API after requiring e."
  (require 'e)
  (should (fboundp 'e-harness-create))
  (should (fboundp 'e-harness-prompt))
  (should (fboundp 'e-harness-messages)))

(provide 'e-test)

;;; e-test.el ends here
