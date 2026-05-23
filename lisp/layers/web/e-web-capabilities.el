;;; e-web-capabilities.el --- Web capabilities for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capability constructor and reference resources for web access.

;;; Code:

(require 'e-capabilities)
(require 'e-store)
(require 'e-web-tools)

(defconst e-web-instructions
  "Web access is available. Use search for queries, fetch for passive page reads, and browser for rendered/interactive pages. Read e://web/refs/overview.md for details when needed."
  "Compact model-facing instructions for the web capability.")

(defconst e-web--reference-resources
  '(("refs/overview.md"
     "Web overview"
     "# Web access overview

Use `web_search` for search queries, `web_fetch` for passive HTTP reads, and browser tools for rendered or interactive pages. Prefer search before fetch when you need to discover sources. Prefer fetch before browser when a normal HTTP read is enough.")
    ("refs/search.md"
     "Web search reference"
     "# Web search

Use `web_search` for query discovery. It calls the configured bx web backend directly and returns normalized results with rank, title, URL, snippet, source, and date when available.")
    ("refs/fetch.md"
     "Web fetch reference"
     "# Web fetch

Use `web_fetch` for passive HTTP or HTTPS page reads. It does not execute JavaScript. Use browser tools when rendering, interaction, login flows, or dynamic DOM state matter.")
    ("refs/browser.md"
     "Web browser reference"
     "# Browser tools

Use browser tools for rendered and interactive pages. Available provider-facing tools are `web_browser_open`, `web_browser_observe`, `web_browser_click`, `web_browser_type`, `web_browser_press`, `web_browser_screenshot`, and `web_browser_close`.")
    ("refs/boundaries.md"
     "Web boundaries"
     "# Web boundaries

The MVP keeps web access narrow. It does not implement downloads, uploads, clipboard, geolocation, camera, microphone, notifications, or a permission system. Unsupported browser operations should return clear errors."))
  "Reference resources exposed under e://web/refs/.")

(defun e-web--register-reference-resources (store capability)
  "Register web reference resources for CAPABILITY in STORE."
  (dolist (resource e-web--reference-resources)
    (pcase-let ((`(,path ,description ,content) resource))
      (e-store-register
       store
       (e-capability-id capability)
       path
       :description description
       :content content))))

(defun e-web-access-capability-create ()
  "Create the basic web access capability."
  (e-capability-create
   :id 'web
   :name "Web Access"
   :instructions e-web-instructions
   :tools (list #'e-web-tools-register-search
                #'e-web-tools-register-fetch
                #'e-web-tools-register-browser)
   :resources (list #'e-web--register-reference-resources)))

(provide 'e-web-capabilities)

;;; e-web-capabilities.el ends here
