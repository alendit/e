;;; e-action-resources.el --- Read-only action description resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Generated read-only resources for active capability action contracts.
;; These resources describe actions.  They do not execute actions.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-operations)
(require 'e-harness)
(require 'e-resource-patterns)
(require 'e-resource-query)
(require 'e-resources)

(define-error 'e-action-resources-invalid-uri
  "Action description resource URI is invalid")
(define-error 'e-action-resources-not-found
  "Action description resource was not found")

(defun e-action-resources--keyword-name (keyword)
  "Return KEYWORD without its leading colon."
  (string-remove-prefix ":" (symbol-name keyword)))

(defun e-action-resources--action-key-from-path (path)
  "Return action keyword from URI PATH segment."
  (intern (concat ":" path)))

(defun e-action-resources--action-segment (action-key)
  "Return URI segment for ACTION-KEY."
  (e-action-resources--keyword-name action-key))

(defun e-action-resources--capability-segment (capability)
  "Return URI segment for CAPABILITY."
  (symbol-name (e-capability-id capability)))

(defun e-action-resources--action-uri (capability action-key)
  "Return the e-action URI for CAPABILITY ACTION-KEY."
  (format "e-action://%s/%s"
          (e-action-resources--capability-segment capability)
          (e-action-resources--action-segment action-key)))

(defun e-action-resources--capability-uri (capability)
  "Return the e-action URI for CAPABILITY."
  (format "e-action://%s" (e-action-resources--capability-segment capability)))

(defun e-action-resources--effective-capabilities (harness session-id turn-id)
  "Return action-bearing effective capabilities for HARNESS SESSION-ID TURN-ID."
  (cl-remove-if-not
   (lambda (capability)
     (e-capability-actions capability))
   (e-harness-effective-capabilities harness session-id turn-id)))

(defun e-action-resources--action-entries (capability)
  "Return action entries for CAPABILITY as (KEY . SPEC) pairs."
  (let ((actions (e-capability-actions capability))
        entries)
    (while actions
      (let ((key (pop actions))
            (spec (pop actions)))
        (push (cons key spec) entries)))
    (nreverse entries)))

(defun e-action-resources--find-capability
    (harness session-id turn-id capability-id)
  "Return active CAPABILITY-ID for HARNESS SESSION-ID TURN-ID."
  (cl-find-if (lambda (capability)
                (eq (e-capability-id capability) capability-id))
              (e-harness-effective-capabilities harness session-id turn-id)))

(defun e-action-resources--find-action (capability action-key)
  "Return action spec for CAPABILITY ACTION-KEY."
  (cdr (assq action-key (e-action-resources--action-entries capability))))

(defun e-action-resources--schema-string (schema)
  "Return readable SCHEMA text."
  (if schema
      (prin1-to-string schema)
    "nil"))

(defun e-action-resources--action-description (spec)
  "Return human-facing description for action SPEC."
	  (cond
	   ((e-action-p spec)
	    (let ((description (e-action-description spec)))
	      (if (stringp description)
	          description
	        "No description provided.")))
	   (t
	    "Invalid action descriptor.")))

(defun e-action-resources--action-requires-session-p (spec)
  "Return non-nil when SPEC requires a session."
  (and (e-action-p spec) (e-action-requires-session spec)))

(defun e-action-resources--action-parameters (spec)
  "Return parameter schema for SPEC."
  (and (e-action-p spec) (e-action-parameters spec)))

(defun e-action-resources--format-action-summary
    (capability action-key spec)
  "Return one-line summary for CAPABILITY ACTION-KEY SPEC."
	  (format "- %s :: %s%s"
	          (e-action-resources--action-uri capability action-key)
	          (e-action-resources--action-description spec)
	          (if (e-action-p spec)
	              ""
	            " [invalid]")))

(defun e-action-resources--format-action-contract
    (capability action-key spec)
  "Return read-only contract text for CAPABILITY ACTION-KEY SPEC."
  (let ((capability-id (e-capability-id capability)))
    (string-join
     (list
      (format "# Action %s/%s"
              capability-id
              (e-action-resources--keyword-name action-key))
      ""
      (format "Capability: %s" capability-id)
      (format "Capability name: %s" (or (e-capability-name capability) ""))
      (format "Action: %s" action-key)
      (format "URI: %s" (e-action-resources--action-uri capability action-key))
      ""
      "Description:"
      (e-action-resources--action-description spec)
      ""
      (format "Requires session: %s"
              (if (e-action-resources--action-requires-session-p spec)
                  "true"
                "false"))
      (format "Descriptor: %s" (if (e-action-p spec) "true" "false"))
      ""
      "Parameter schema:"
      (e-action-resources--schema-string
       (e-action-resources--action-parameters spec))
      ""
      "Call form:"
      (format "(e-actions-call '%s %s ARGUMENTS)" capability-id action-key)
      ""
      "This resource is read-only. It describes the action contract; it does not execute the action.")
     "\n")))

(defun e-action-resources--format-capability (capability)
  "Return description text for CAPABILITY actions."
  (let ((entries (e-action-resources--action-entries capability)))
    (string-join
     (append
      (list
       (format "# Capability actions: %s" (e-capability-id capability))
       ""
       (format "Capability name: %s" (or (e-capability-name capability) ""))
       (format "URI: %s" (e-action-resources--capability-uri capability))
       ""
       "Actions:")
      (mapcar (lambda (entry)
                (e-action-resources--format-action-summary
                 capability (car entry) (cdr entry)))
              entries)
      (list
       ""
       "Read an action URI for its call contract. These resources are read-only."))
     "\n")))

(defun e-action-resources--format-active (harness session-id turn-id)
  "Return active action overview for HARNESS SESSION-ID TURN-ID."
  (let ((capabilities
         (e-action-resources--effective-capabilities harness session-id turn-id)))
    (string-join
     (append
      (list
       "# Active action descriptions"
       ""
       "These resources describe active capability actions. They do not execute actions."
       "Use e-actions-call from Elisp to execute an action."
       ""
       "Resources:")
      (if capabilities
          (mapcar (lambda (capability)
                    (format "- %s :: %s"
                            (e-action-resources--capability-uri capability)
                            (or (e-capability-name capability) "")))
                  capabilities)
        (list "- none")))
     "\n")))

(defun e-action-resources--all-resource-records (harness session-id turn-id)
  "Return generated e-action resource records for HARNESS SESSION-ID TURN-ID."
  (let ((records (list (list :uri "e-action://active"
                             :name "active"
                             :kind 'file
                             :content (e-action-resources--format-active
                                       harness session-id turn-id))))
        (capabilities
         (e-action-resources--effective-capabilities harness session-id turn-id)))
    (dolist (capability capabilities)
      (push (list :uri (e-action-resources--capability-uri capability)
                  :name (e-action-resources--capability-segment capability)
                  :kind 'directory
                  :content (e-action-resources--format-capability capability))
            records)
      (dolist (entry (e-action-resources--action-entries capability))
        (push (let ((action-key (car entry))
                    (spec (cdr entry)))
                (list :uri (e-action-resources--action-uri capability action-key)
                      :name (format "%s/%s"
                                    (e-action-resources--capability-segment
                                     capability)
                                    (e-action-resources--action-segment
                                     action-key))
                      :kind 'file
                      :content (e-action-resources--format-action-contract
                                capability action-key spec)))
              records)))
    (nreverse records)))

(defun e-action-resources--require-context (harness)
  "Signal unless HARNESS is valid."
  (unless (e-harness-p harness)
    (signal 'e-action-resources-invalid-uri
            (list "e-action resources require an active harness"))))

(defun e-action-resources--read (harness session-id turn-id uri _range)
  "Read action description URI for HARNESS SESSION-ID TURN-ID."
  (e-action-resources--require-context harness)
  (let* ((address (plist-get uri :address))
         (segments (split-string address "/" t)))
    (pcase segments
      (`("active")
       (e-action-resources--format-active harness session-id turn-id))
      (`(,capability-name)
       (let* ((capability-id (intern capability-name))
              (capability (e-action-resources--find-capability
                           harness session-id turn-id capability-id)))
         (unless (and capability (e-capability-actions capability))
           (signal 'e-action-resources-not-found
                   (list (format "No active action capability: %s"
                                 capability-name))))
         (e-action-resources--format-capability capability)))
      (`(,capability-name ,action-name)
       (let* ((capability-id (intern capability-name))
              (action-key (e-action-resources--action-key-from-path action-name))
              (capability (e-action-resources--find-capability
                           harness session-id turn-id capability-id))
              (spec (and capability
                         (e-action-resources--find-action capability action-key))))
         (unless spec
           (signal 'e-action-resources-not-found
                   (list (format "No active action: %s/%s"
                                 capability-name action-name))))
         (e-action-resources--format-action-contract
          capability action-key spec)))
      (_
       (signal 'e-action-resources-invalid-uri
               (list (format "Invalid e-action URI: %s"
                             (plist-get uri :uri))))))))

(defun e-action-resources--scope-prefix (address)
  "Return glob scope prefix for ADDRESS."
  (cond
   ((string-empty-p address) nil)
   ((string= address "active") "active")
   (t (string-remove-suffix "/" address))))

(defun e-action-resources--record-in-scope-p (record scope)
  "Return non-nil when RECORD is under SCOPE."
  (or (null scope)
      (equal (plist-get record :name) scope)
      (string-prefix-p (concat scope "/") (plist-get record :name))))

(defun e-action-resources--query-field-functions ()
  "Return e-action:// resource query field functions."
  `(("name" . ,(lambda (record) (plist-get record :name)))
    ("uri" . ,(lambda (record) (plist-get record :uri)))))

(defun e-action-resources--query-records (records &rest arguments)
  "Apply e-action:// query controls to RECORDS using ARGUMENTS."
  (apply #'e-resource-query-apply
         records
         "e-action"
         '("default" "name" "uri")
         nil
         :field-functions (e-action-resources--query-field-functions)
         arguments))

(defun e-action-resources--resource-record (record)
  "Return public resource result for action RECORD."
  (list :uri (plist-get record :uri)
        :name (plist-get record :name)
        :kind (plist-get record :kind)))

(defun e-action-resources--glob
    (harness session-id turn-id uri pattern limit case-sensitive
             &optional sort-by sort-order created-after created-before
             updated-after updated-before)
  "Glob action description resources for HARNESS SESSION-ID TURN-ID."
  (e-action-resources--require-context harness)
  (let* ((scope (e-action-resources--scope-prefix (plist-get uri :address)))
         (actual-limit (or limit 1000))
         (actual-pattern (or pattern "**"))
         (actual-case-sensitive (if (null case-sensitive) t case-sensitive))
         (records (cl-remove-if-not
                   (lambda (record)
                     (and (e-action-resources--record-in-scope-p record scope)
                          (e-resource-pattern-glob-match-p
                           actual-pattern
                           (plist-get record :name)
                           actual-case-sensitive)))
                   (e-action-resources--all-resource-records
                    harness session-id turn-id)))
         (queried (e-action-resources--query-records
                   records
                   :sort-by sort-by
                   :sort-order sort-order
                   :created-after created-after
                   :created-before created-before
                   :updated-after updated-after
                   :updated-before updated-before))
         (selected (seq-take queried actual-limit)))
    (list :resources
          (vconcat (mapcar #'e-action-resources--resource-record selected))
          :truncated (> (length queried) actual-limit))))

(defun e-action-resources--search-record (record query options)
  "Return matches for QUERY in RECORD using OPTIONS."
  (let* ((case-fold-search (not (plist-get options :case-sensitive)))
         (regexp (e-resource-pattern-search-emacs-regexp query options))
         (lines (split-string (plist-get record :content) "\n"))
         (line-number 1)
         matches)
    (dolist (line lines)
      (when (string-match regexp line)
        (push (list :uri (plist-get record :uri)
                    :line line-number
                    :column (1+ (match-beginning 0))
                    :text line)
              matches))
      (setq line-number (1+ line-number)))
    (nreverse matches)))

(defun e-action-resources--search (harness session-id turn-id uri query options)
  "Search action description resources for QUERY."
  (e-action-resources--require-context harness)
  (let* ((scope (e-action-resources--scope-prefix (plist-get uri :address)))
         (actual-limit (or (plist-get options :limit) 1000))
         (glob-pattern (plist-get options :glob))
         (records (cl-remove-if-not
                   (lambda (record)
                     (and (e-action-resources--record-in-scope-p record scope)
                          (or (null glob-pattern)
                              (e-resource-pattern-glob-match-p
                               glob-pattern
                               (plist-get record :name)
                               t))))
                   (e-action-resources--all-resource-records
                    harness session-id turn-id)))
         matches)
    (setq records
          (e-resource-query-apply-search
           records
           "e-action"
           '("default" "name" "uri")
           nil
           options
           (e-action-resources--query-field-functions)))
    (dolist (record records)
      (setq matches
            (append matches
                    (e-action-resources--search-record record query options))))
    (list :matches (vconcat (seq-take matches actual-limit))
          :truncated (> (length matches) actual-limit))))

(cl-defun e-action-resources-register-resource-methods
    (registry &key harness session-id turn-id &allow-other-keys)
  "Register e-action:// resource methods in REGISTRY."
  (dolist (method
           (list
            (e-resource-method-create
             :scheme "e-action"
             :operation e-operation-read
             :description "Read generated descriptions of active capability actions."
             :uri-patterns '("e-action://active"
                             "e-action://<capability>"
                             "e-action://<capability>/<action>")
             :handler (lambda (uri range)
                        (e-action-resources--read
                         harness session-id turn-id uri range)))
            (e-resource-method-create
             :scheme "e-action"
             :operation e-operation-glob
             :description "List generated active action description resources."
             :uri-patterns '("e-action://"
                             "e-action://<capability>")
             :handler (lambda (uri pattern limit case-sensitive sort-by sort-order
                               created-after created-before updated-after updated-before)
                        (e-action-resources--glob
                         harness session-id turn-id
                         uri pattern limit case-sensitive sort-by sort-order
                         created-after created-before updated-after updated-before)))
            (e-resource-method-create
             :scheme "e-action"
             :operation e-operation-search
             :description "Search generated active action descriptions."
             :uri-patterns '("e-action://"
                             "e-action://<capability>")
             :handler (lambda (uri query options)
                        (e-action-resources--search
                         harness session-id turn-id uri query options)))))
    (e-resources-register registry method)))

(defun e-action-resources-capability-create ()
  "Create the action description resource capability."
  (e-capability-create
   :id 'action-descriptions
   :name "Action Descriptions"
   :resource-methods
   (list (e-capability-resource-method-provider-create
          :handler #'e-action-resources-register-resource-methods))))

(provide 'e-action-resources)

;;; e-action-resources.el ends here
