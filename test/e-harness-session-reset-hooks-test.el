;;; e-harness-session-reset-hooks-test.el --- Session reset hook tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Focused tests for harness session-reset lifecycle hooks.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-hooks)

(ert-deftest e-harness-session-reset-hooks-test-runs-active-hook ()
  "Harness reset runs active :session-reset hooks with narrow context."
  (let* ((seen nil)
         (capability
          (e-capability-create
           :id 'session-reset-hook
           :name "Session Reset Hook"
           :hooks
           (list
            (e-hook-create
             :id "50-capture-session-reset"
             :point :session-reset
             :handler (lambda (value context)
                        (setq seen
                              (list :value value
                                    :harness (plist-get context :harness)
                                    :session-id
                                    (plist-get context :session-id)))
                        value)))))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities (list capability))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-reset harness "session-1")
    (should (equal (plist-get seen :value) nil))
    (should (eq (plist-get seen :harness) harness))
    (should (equal (plist-get seen :session-id) "session-1"))))

(provide 'e-harness-session-reset-hooks-test)

;;; e-harness-session-reset-hooks-test.el ends here
