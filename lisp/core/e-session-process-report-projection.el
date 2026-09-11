;;; e-session-process-report-projection.el --- Process-report query facts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure derivation of the bounded relational query facts attached to one
;; canonical process-report journal record.  This module knows no SQLite and
;; retains no report collection; both the runtime writer and explicit offline
;; upgrader use the same derivation.

;;; Code:

(require 'cl-lib)

(define-error 'e-session-process-report-projection-error
  "Invalid process-report query projection")

(defconst e-session-process-report-projection-association-limit 64
  "Maximum distinct marker associations on one process report.")
(defconst e-session-process-report-projection-scalar-byte-limit 4096
  "Maximum encoded bytes in one process-report projection scalar.")

(defun e-session-process-report-projection--error (message &rest data)
  "Signal bounded projection error MESSAGE with diagnostic DATA."
  (signal 'e-session-process-report-projection-error (cons message data)))

(defun e-session-process-report-projection--proper-plist-p (value)
  "Return non-nil when VALUE is a proper keyword plist with unique keys."
  (and (proper-list-p value)
       (let ((tail value) keys valid)
         (setq valid t)
         (while (and valid tail)
           (let ((key (pop tail)))
             (setq valid
                   (and (keywordp key) (consp tail) (not (memq key keys))))
             (when valid
               (push key keys)
               (pop tail))))
         valid)))

(defun e-session-process-report-projection--scalar (value field &optional nil-ok)
  "Return bounded string VALUE for FIELD, accepting nil when NIL-OK."
  (unless (or (and nil-ok (null value))
              (and (stringp value)
                   (> (string-bytes value) 0)
                   (<= (string-bytes value)
                       e-session-process-report-projection-scalar-byte-limit)))
    (e-session-process-report-projection--error
     "Process-report projection scalar is invalid" :field field))
  value)

(defun e-session-process-report-projection--report-type (report)
  "Return normalized report type for REPORT."
  (let* ((raw (plist-get report :report-type))
         (value (cond ((stringp raw) raw)
                      ((symbolp raw) (symbol-name raw))
                      (t nil))))
    (e-session-process-report-projection--scalar value :report-type)))

(defun e-session-process-report-projection--marker-ids (report report-type)
  "Return sorted distinct marker ids from REPORT of REPORT-TYPE."
  (let* ((raw
          (cond
           ((member report-type '("marker" "triage"))
            (list (plist-get report :marker-id)))
           ((equal report-type "extraction")
            (let ((value (plist-get report :marker-ids)))
              (cond ((vectorp value) (append value nil))
                    ((proper-list-p value) value)
                    (t
                     (e-session-process-report-projection--error
                      "Process-report marker association set is invalid"
                      :field :marker-ids)))))
           (t nil)))
         normalized)
    (when (> (length raw)
             e-session-process-report-projection-association-limit)
      (e-session-process-report-projection--error
       "Process-report marker association count exceeds its raw bound"
       :field :marker-ids
       :limit e-session-process-report-projection-association-limit))
    (dolist (marker-id raw)
      (push
       (copy-sequence
        (e-session-process-report-projection--scalar marker-id :marker-id))
       normalized))
    (setq normalized (sort (delete-dups normalized) #'string<))
    (when (> (length normalized)
             e-session-process-report-projection-association-limit)
      (e-session-process-report-projection--error
       "Process-report marker association count exceeds its bound"
       :field :marker-ids
       :limit e-session-process-report-projection-association-limit))
    normalized))

(defun e-session-process-report-projection--triage-status
    (report report-type)
  "Return REPORT's normalized status when REPORT-TYPE is triage."
  (when (equal report-type "triage")
    (let* ((raw (plist-get report :status))
           (status (cond ((stringp raw) (downcase raw))
                         ((symbolp raw) (downcase (symbol-name raw)))
                         (t nil))))
      (e-session-process-report-projection--scalar status :status))))

(defun e-session-process-report-projection-rows (record)
  "Return detached relational projection rows for canonical RECORD.

Non-process-report records return nil.  A process report always produces one
ordinal-zero base row.  Marker associations are deduplicated and sorted before
receiving stable ordinals.  The returned values contain no canonical payload."
  (when (equal (plist-get record :type) "process-report")
    (let* ((report (plist-get record :report))
           (_ (unless (e-session-process-report-projection--proper-plist-p report)
                (e-session-process-report-projection--error
                 "Process-report payload is not a proper plist"
                 :field :report)))
           (report-type
            (e-session-process-report-projection--report-type report))
           (request-id
            (when (equal report-type "request-shape")
              (e-session-process-report-projection--scalar
               (plist-get report :provider-request-id)
               :provider-request-id)))
           (marker-ids
            (e-session-process-report-projection--marker-ids
             report report-type))
           (triage-status
            (e-session-process-report-projection--triage-status
             report report-type))
           (rows
            (list (list :association-ordinal 0
                        :report-type report-type
                        :marker-id nil
                        :provider-request-id request-id
                        :triage-status nil)))
           (ordinal 0))
      (dolist (marker-id marker-ids)
        (setq ordinal (1+ ordinal)
              rows
              (append rows
                      (list (list :association-ordinal ordinal
                                  :report-type report-type
                                  :marker-id marker-id
                                  :provider-request-id nil
                                  :triage-status triage-status)))))
      rows)))

(provide 'e-session-process-report-projection)

;;; e-session-process-report-projection.el ends here
