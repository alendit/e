;;; e-resource-discovery.el --- Advanced resource discovery reference for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; On-demand documentation for advanced glob, search, and outline controls.
;; The model-facing schemas stay lean because common calls need only a URI and
;; usually a pattern or query.

;;; Code:

(require 'subr-x)
(require 'e-capabilities)
(require 'e-layers)
(require 'e-skills)

(defconst e-resource-discovery-reference
  (string-join
   '("# Resource discovery tools"
     ""
     "Use the tool schemas directly for ordinary calls. This reference covers advanced controls."
     ""
     "## glob"
     ""
     "Required: `uri`. Optional: `pattern`, `case-sensitive`, `limit`, `sort-by`, `sort-order`, `created-after`, `created-before`, `updated-after`, `updated-before`."
     "Patterns match beneath the URI root. Case-sensitive matching is the default. Time filters are inclusive ISO 8601 values and apply only when the resource scheme exposes matching metadata. Explicit sort fields accept `asc` or `desc`."
     ""
     "## search"
     ""
     "Required: `uri`, `query`. Optional: `glob`, `case-sensitive`, `whole-word`, `multiline`, `limit`, `resource-sort-by`, `resource-sort-order`, `resource-limit`, and the same created/updated time bounds as `glob`."
     "Queries are ranked lexical searches. Whitespace-separated terms must all match; `*` is a non-whitespace wildcard. `glob` limits candidate resources. Resource sorting and `resource-limit` are applied before text matches are returned."
     ""
     "## table_of_content"
     ""
     "Required: `uri`. Optional: `max-depth`, `max-items`, `min-lines`, `format`, `language`, `lenient`."
     "The tool uses wot. File-backed resources are outlined from their backing file when safe; in-memory resources use stdin. `format` is `markdown` or `json`. Set `language` when an in-memory resource has no inferable language. `session://` is unsupported."
     ""
     "## Active URI schemes"
     ""
     "The available schemes depend on active capabilities. Common roots are `file://`, `buffer://`, `tmp://`, `e://`, `e-action://`, `raw-result://`, and `session://`. A scheme may support only some discovery operations or advanced controls.")
   "\n")
  "On-demand reference for advanced resource discovery controls.")

(defun e-resource-discovery-capability-create ()
  "Create the resource discovery reference capability."
  (e-capability-with-skills-create
   :id 'resource-discovery
   :name "Resource Discovery"
   :skills (list
            (e-skill-spec-create
             :name "resource-discovery"
             :description "Advanced glob, search, and table_of_content controls."
             :content e-resource-discovery-reference))))

(defun e-resource-discovery-layer-create ()
  "Create the resource-discovery layer."
  (e-layer-create
   :id 'resource-discovery
   :name "Resource Discovery"
   :capabilities (list (e-resource-discovery-capability-create))))

(provide 'e-resource-discovery)

;;; e-resource-discovery.el ends here
