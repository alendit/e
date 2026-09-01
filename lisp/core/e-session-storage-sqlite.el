;;; e-session-storage-sqlite.el --- SQLite session physical adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Implements the SQLite side of the existing session-storage seam.  It maps
;; consumer-shaped session operations to the generic runtime-store protocol;
;; schema, SQL, worker lifecycle, aggregate replay, and catalog policy remain
;; with their existing owners.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'e-runtime-store)

(defvar e-session-storage-sqlite--backends
  (make-hash-table :test 'eq :weakness 'key))
(defvar e-session-storage-sqlite--runtimes
  (make-hash-table :test 'eq :weakness 'key))
(defvar e-session-storage-sqlite--owned-runtimes
  (make-hash-table :test 'eq :weakness 'key))

(defun e-session-storage-sqlite-register (store backend runtime owns-runtime)
  "Register STORE's physical BACKEND and optional RUNTIME."
  (puthash store backend e-session-storage-sqlite--backends)
  (if runtime
      (puthash store runtime e-session-storage-sqlite--runtimes)
    (remhash store e-session-storage-sqlite--runtimes))
  (if (and runtime owns-runtime)
      (puthash store t e-session-storage-sqlite--owned-runtimes)
    (remhash store e-session-storage-sqlite--owned-runtimes)))

(defun e-session-storage-sqlite-store-p (store)
  "Return non-nil when STORE uses the opt-in SQLite adapter."
  (eq (gethash store e-session-storage-sqlite--backends 'legacy) 'sqlite))

(defun e-session-storage-sqlite-runtime (store)
  "Return STORE's runtime-store adapter, or nil."
  (gethash store e-session-storage-sqlite--runtimes))

(defun e-session-storage-sqlite--call (store kind body &optional id)
  "Call STORE's typed runtime KIND BODY operation."
  (let ((runtime (e-session-storage-sqlite-runtime store)))
    (unless runtime
      (signal 'e-session-storage-error (list "SQLite runtime is unavailable")))
    (e-runtime-store-call runtime kind body id)))

(defun e-session-storage-sqlite-reference (session-id)
  "Return the opaque catalog reference for SESSION-ID."
  (format "sqlite:session:%s" session-id))

(defun e-session-storage-sqlite-header (store session-id)
  "Return STORE's physical header for SESSION-ID."
  (let ((header (e-session-storage-sqlite--call
                 store 'read (list :op 'session-header :session-id session-id))))
    (plist-put header :stored-bytes (plist-get header :byte-size))
    ;; The facade's legacy cursor is named byte-size; SQLite uses position.
    (plist-put header :byte-size (plist-get header :revision))
    header))

(defun e-session-storage-sqlite-read-checkpoint (store session-id)
  "Return SESSION-ID's exact checkpoint or signal when absent."
  (let ((result (e-session-storage-sqlite--call
                 store 'read (list :op 'checkpoint-get :session-id session-id))))
    (if result
        (let ((value (plist-get result :value)))
          (when (vectorp (plist-get value :records))
            (plist-put value :records (append (plist-get value :records) nil)))
          value)
      (signal 'file-missing (list "SQLite checkpoint" session-id)))))

(defun e-session-storage-sqlite-checkpoint-present-p (store session-id)
  "Return non-nil when SESSION-ID has a checkpoint."
  (and (e-session-storage-sqlite--call
        store 'read (list :op 'checkpoint-get :session-id session-id)) t))

(defun e-session-storage-sqlite-write-checkpoint (store session-id value)
  "Persist SESSION-ID checkpoint VALUE."
  (e-session-storage-sqlite--call
   store 'write (list :op 'checkpoint-put :session-id session-id :value value)))

(defun e-session-storage-sqlite-read-records (store session-id &optional after)
  "Return all semantic records for SESSION-ID after AFTER."
  (let ((position (or after 0)) records next)
    (while
        (progn
          (let ((page (e-session-storage-sqlite-read-page
                       store session-id position 512)))
            (setq records
                  (nconc records
                         (mapcar (lambda (entry) (plist-get entry :value))
                                 (plist-get page :records)))
                  next (plist-get page :next)
                  position (or next position)))
          next))
    records))

(defun e-session-storage-sqlite-read-page (store session-id after limit)
  "Return one bounded semantic page for SESSION-ID."
  (e-session-storage-sqlite--call
   store 'read (list :op 'session-record-page :session-id session-id
                     :after (or after 0) :limit (or limit 256))))

(defun e-session-storage-sqlite-session-ids (store)
  "Return STORE's durable session identities."
  (e-session-storage-sqlite--call store 'read '(:op session-ids)))

(defun e-session-storage-sqlite-read-catalog (store)
  "Return STORE's catalog projection, or nil."
  (when-let* ((result (e-session-storage-sqlite--call
                       store 'read '(:op catalog-get))))
    (plist-get result :value)))

(defun e-session-storage-sqlite-write-catalog (store value)
  "Persist STORE catalog projection VALUE."
  (e-session-storage-sqlite--call
   store 'write (list :op 'catalog-put :value value)))

(defun e-session-storage-sqlite-status (store)
  "Return bounded adapter and runtime status for STORE."
  (append (list :backend 'sqlite :unsettled-write-count 0)
          (e-runtime-store-status (e-session-storage-sqlite-runtime store))))

(defun e-session-storage-sqlite-append (store session-id record)
  "Append one exact RECORD to SESSION-ID."
  (e-session-storage-sqlite--call
   store 'write (list :op 'session-append :session-id session-id
                      :record record)))

(defun e-session-storage-sqlite-append-batch (store session-id records)
  "Append exact RECORDS atomically to SESSION-ID."
  (e-session-storage-sqlite--call
   store 'write (list :op 'session-append-batch :session-id session-id
                      :records (vconcat records))))

(defun e-session-storage-sqlite-prepare-append-batch (session-id records)
  "Return detached RECORDS after preflighting one complete batch frame."
  (let* ((records (mapcar #'copy-tree records))
         (body (list :op 'session-append-batch :session-id session-id
                     :records (vconcat records))))
    ;; Runtime submission performs the same canonical encoding before queueing.
    ;; Doing it here proves the complete fork frame before any durable effect.
    (e-runtime-store-codec-encode body)
    records))

(defun e-session-storage-sqlite-delete (store session-id)
  "Delete SESSION-ID and its physically subordinate private state."
  (e-session-storage-sqlite--call
   store 'write (list :op 'session-delete :session-id session-id)))

(defun e-session-storage-sqlite-ordered-barrier (store)
  "Return after STORE acknowledges all previously submitted effects.

The runtime store serializes writes and gives queued commits priority over
bounded reads.  Its status query is therefore the narrow ordered barrier: an
acknowledged status response cannot overtake an earlier submitted write."
  (e-session-storage-sqlite--call store 'read '(:op status)))

(defun e-session-storage-sqlite-healthy-p (store)
  "Return non-nil after STORE acknowledges its ordered health barrier."
  (e-session-storage-sqlite-ordered-barrier store))

(defun e-session-storage-sqlite-tool-transition
    (store session-id call-id state payload expected-revision)
  "Persist one tool continuity transition."
  (e-session-storage-sqlite--call
   store 'write
   (append (list :op 'tool-transition :session-id session-id
                 :call-id call-id :state state :payload payload)
           (when expected-revision
             (list :expected-revision expected-revision)))))

(defun e-session-storage-sqlite-tool-classifications (store session-id)
  "Return SESSION-ID's bounded tool continuity classifications."
  (e-session-storage-sqlite--call
   store 'read (list :op 'tool-list :session-id session-id :limit 512)))

(defun e-session-storage-sqlite-close (store)
  "Close STORE's subordinate runtime worker."
  (when-let* ((runtime (and (gethash store
                                    e-session-storage-sqlite--owned-runtimes)
                            (e-session-storage-sqlite-runtime store))))
    (e-runtime-store-close runtime))
  (remhash store e-session-storage-sqlite--owned-runtimes)
  (remhash store e-session-storage-sqlite--runtimes)
  (remhash store e-session-storage-sqlite--backends)
  t)

(provide 'e-session-storage-sqlite)

;;; e-session-storage-sqlite.el ends here
