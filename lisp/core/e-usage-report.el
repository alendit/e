;;; e-usage-report.el --- Usage reports over durable e activity -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Aggregates durable session activity into tool and action usage rows.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-session)
(require 'e-tools)

(defun e-usage-report--increment (table key)
  "Increment KEY in TABLE."
  (puthash key (1+ (gethash key table 0)) table))

(defun e-usage-report--tool-name (event)
  "Return tool name from activity EVENT."
  (let ((payload (plist-get event :payload)))
    (or (plist-get payload :name)
        (plist-get (plist-get payload :tool-call) :name))))

(defun e-usage-report--action-name (event)
  "Return capability/action name from activity EVENT."
  (let* ((payload (plist-get event :payload))
         (capability (plist-get payload :capability-id))
         (action (plist-get payload :action)))
    (when (and capability action)
      (format "%s/%s"
              capability
              (if (keywordp action)
                  (substring (symbol-name action) 1)
                (symbol-name action))))))

(defun e-usage-report--event-counts (events)
  "Return tool and action count tables from activity EVENTS."
  (let ((tools (make-hash-table :test 'equal))
        (actions (make-hash-table :test 'equal)))
    (dolist (event events)
      (pcase (plist-get event :event-type)
        ('tool-started
         (when-let ((name (e-usage-report--tool-name event)))
           (e-usage-report--increment tools name)))
        ('action-started
         (when-let ((name (e-usage-report--action-name event)))
           (e-usage-report--increment actions name)))))
    (list :tools tools :actions actions)))

(defun e-usage-report--row (name count)
  "Return one usage row for NAME and COUNT."
  (list :name name :count count))

(defun e-usage-report--sorted-rows (table)
  "Return sorted rows from TABLE."
  (let (rows)
    (maphash (lambda (name count)
               (push (e-usage-report--row name count) rows))
             table)
    (sort rows
          (lambda (left right)
            (let ((left-count (plist-get left :count))
                  (right-count (plist-get right :count)))
              (if (= left-count right-count)
                  (string< (plist-get left :name) (plist-get right :name))
                (> left-count right-count)))))))

(defun e-usage-report--merge-zero (table names)
  "Ensure each entry in NAMES appears in TABLE with a zero value."
  (dolist (name names)
    (unless (gethash name table)
      (puthash name 0 table)))
  table)

(defun e-usage-report--active-tool-names (harness session-id turn-id)
  "Return active tool names for HARNESS SESSION-ID TURN-ID."
  (mapcar (lambda (definition)
            (plist-get definition :name))
          (e-tools-definitions
           (e-harness-tools harness session-id turn-id))))

(defun e-usage-report--action-key-name (key)
  "Return display name for action KEY."
  (if (keywordp key)
      (substring (symbol-name key) 1)
    (symbol-name key)))

(defun e-usage-report--active-action-names (harness session-id turn-id)
  "Return active action names for HARNESS SESSION-ID TURN-ID."
  (let (names)
    (dolist (capability (e-harness-effective-capabilities
                         harness session-id turn-id))
      (let ((actions (e-capability-actions capability)))
        (while actions
          (let ((key (pop actions)))
            (pop actions)
            (push (format "%s/%s"
                          (e-capability-id capability)
                          (e-usage-report--action-key-name key))
                  names)))))
    (nreverse names)))

(cl-defun e-usage-report-session
    (harness session-id &key turn-id include-zero-rows)
  "Return usage report for HARNESS SESSION-ID.
When TURN-ID is non-nil, count only activity from that turn.
When INCLUDE-ZERO-ROWS is non-nil, include active tools and actions with count
zero."
  (let* ((events (e-session-activity-events (e-harness-sessions harness)
                                            session-id))
         (events (if turn-id
                     (cl-remove-if-not
                      (lambda (event)
                        (equal (plist-get event :turn-id) turn-id))
                      events)
                   events))
         (counts (e-usage-report--event-counts events))
         (tools (plist-get counts :tools))
         (actions (plist-get counts :actions)))
    (when include-zero-rows
      (e-usage-report--merge-zero
       tools
       (e-usage-report--active-tool-names harness session-id turn-id))
      (e-usage-report--merge-zero
       actions
       (e-usage-report--active-action-names harness session-id turn-id)))
    (list :tools (e-usage-report--sorted-rows tools)
          :actions (e-usage-report--sorted-rows actions))))

(provide 'e-usage-report)

;;; e-usage-report.el ends here
