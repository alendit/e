;;; e-dev-bytecode-e2e-test.el --- Dev checkout bytecode e2e checks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Opt-in e2e checks for development checkout invariants that can break live
;; Emacs even when source-level unit tests pass.

;;; Code:

(require 'ert)
(require 'e-dev)

(ert-deftest e-dev-bytecode-e2e-test-checkout-has-no-stale-bytecode ()
  "The development checkout must not contain stale source-adjacent bytecode."
  (let ((stale (e-dev-stale-bytecode-files (e-source-directory))))
    (should
     (equal stale nil))))

(provide 'e-dev-bytecode-e2e-test)

;;; e-dev-bytecode-e2e-test.el ends here
