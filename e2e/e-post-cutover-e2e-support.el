;;; e-post-cutover-e2e-support.el --- Disposable post-cutover fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared test-owned legacy-tree and default-runtime fixtures for deterministic
;; post-cutover E2E scenarios.  This module owns no product behavior.

;;; Code:

(require 'json)
(require 'e-session-codec)

(defconst e-post-cutover-e2e--session-count 64
  "Number of legacy sessions in the structural post-cutover fixture.")

(defun e-post-cutover-e2e--write (file text)
  "Write TEXT to test-owned FILE using the retired store encoding."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent)))

(defun e-post-cutover-e2e--session-id (index)
  "Return the deterministic legacy session id for INDEX."
  (format "legacy-%03d" index))

(defun e-post-cutover-e2e--message-content (index)
  "Return the exact fixture message content for INDEX."
  (format "post-cutover message %03d" index))

(defun e-post-cutover-e2e--timestamp (index seconds)
  "Return a deterministic explicit-zone timestamp for INDEX and SECONDS."
  (format "2026-01-%02dT00:00:%02dZ" (1+ (% index 28)) seconds))

(defun e-post-cutover-e2e--write-legacy-session (root index)
  "Write one retired session journal below test-owned ROOT for INDEX."
  (let* ((session-id (e-post-cutover-e2e--session-id index))
         (created-at (e-post-cutover-e2e--timestamp index 0))
         (updated-at (e-post-cutover-e2e--timestamp index 1))
         (root-id (format "root-%03d" index))
         (message-id (format "message-%03d" index))
         (records
          (list
           (list :type "session" :session-id session-id :id root-id
                 :created-at created-at :updated-at updated-at :metadata nil)
           (list :type "message" :session-id session-id :id message-id
                 :parent-id root-id :timestamp updated-at
                 :message
                 (list :role 'user
                       :content (e-post-cutover-e2e--message-content index)
                       :created-at updated-at :type 'message
                       :id message-id :parent-id root-id)))))
    (e-post-cutover-e2e--write
     (expand-file-name
      (format "sessions/sessions/%s.jsonl" session-id) root)
     (concat
      (mapconcat
       (lambda (record)
         (json-encode (e-session-codec-record-for-json record)))
       records "\n")
      "\n"))
    (list :id session-id :created-at created-at :updated-at updated-at
          :message-count 1 :last-message-at updated-at
          :name (format "Legacy session %03d" index))))

(defun e-post-cutover-e2e--make-legacy-root (root)
  "Create the deterministic retired session tree at test-owned ROOT."
  (make-directory root t)
  (let (catalog)
    (dotimes (index e-post-cutover-e2e--session-count)
      (push (e-post-cutover-e2e--write-legacy-session root index) catalog))
    (e-post-cutover-e2e--write
     (expand-file-name "sessions/index.json" root)
     (concat (json-encode (vconcat (nreverse catalog))) "\n")))
  root)

(defun e-post-cutover-e2e--loaded-p (store session-id)
  "Return non-nil when SESSION-ID is loaded in STORE."
  (and (plist-get
        (e-session-aggregate-peek-session store session-id)
        :loaded)
       t))

(provide 'e-post-cutover-e2e-support)

;;; e-post-cutover-e2e-support.el ends here
