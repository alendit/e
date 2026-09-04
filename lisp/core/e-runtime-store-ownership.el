;;; e-runtime-store-ownership.el --- Private runtime-directory claims -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The ordinary and explicit offline SQLite workers share one process-lifetime
;; claim for a runtime directory.  Emacs' file lock is the exclusion authority;
;; the adjacent owner file is bounded diagnostic metadata, never a lease.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(unless (get 'e-runtime-store-worker-error 'error-conditions)
  ;; The private module is normally loaded below a worker-specific definition.
  ;; Keep standalone batch inspection usable without widening a public API.
  (define-error 'e-runtime-store-worker-error "Runtime store worker error"))
(define-error 'e-runtime-store-ownership-error
  "Runtime store ownership failed" 'e-runtime-store-worker-error)
(define-error 'e-runtime-store-owner-active
  "Runtime store already has a live owner" 'e-runtime-store-ownership-error)
(define-error 'e-runtime-store-owner-identity-conflict
  "Runtime store owner identity conflicts" 'e-runtime-store-owner-active)
(define-error 'e-runtime-store-ownership--contention
  "Private noninteractive file-lock contention"
  'e-runtime-store-ownership-error)

(defconst e-runtime-store-ownership-metadata-max-bytes 4096
  "Private maximum size of one runtime ownership diagnostic record.")

(defconst e-runtime-store-ownership-runtime-id-max-chars 256
  "Private maximum length retained for a runtime ownership diagnostic id.")

(defun e-runtime-store-ownership--host-boot-marker ()
  "Return the local host marker that changes at an operating-system boot.

The marker is deliberately independent of Emacs' PID and start time: those
identify a parent *within* a boot, while this value is the evidence that a
persisted parent belongs to a previous host boot.  PID 1's start identity is
available through Emacs on macOS and Linux and changes only when that host (or
its runtime container) boots.  Linux's per-boot UUID is the fallback for a
platform that cannot expose PID 1 attributes."
  (cond
   ((when-let* ((attributes (e-runtime-store-ownership--process-attributes 1))
                (start (e-runtime-store-ownership--process-start attributes)))
      (format "pid-1-start:%S" start)))
   ((file-readable-p "/proc/sys/kernel/random/boot_id")
    (with-temp-buffer
      (insert-file-contents-literally "/proc/sys/kernel/random/boot_id")
      (string-trim (buffer-string))))
   (t
    (signal 'e-runtime-store-ownership-error
            (list "Cannot establish a host boot marker")))))

(defun e-runtime-store-ownership--host-boot-id ()
  "Return a bounded opaque identity for this host operating-system boot."
  (let ((marker (e-runtime-store-ownership--host-boot-marker)))
    (unless (and (stringp marker) (not (string-empty-p marker)))
      (signal 'e-runtime-store-ownership-error
              (list "Host boot marker was empty")))
    ;; Keep platform-specific boot details out of SQLite while retaining a
    ;; stable, process-independent comparison token.
    (secure-hash 'sha256 marker)))

(cl-defstruct (e-runtime-store-ownership-claim
               (:constructor e-runtime-store-ownership-claim--create)
               (:predicate e-runtime-store-ownership-claim-p)
               (:conc-name e-runtime-store-ownership-claim--))
  "One current process claim for a runtime SQLite file."
  database-file metadata-file runtime-id role pid process-start metadata)

(defun e-runtime-store-ownership--metadata-file (database-file)
  "Return the bounded diagnostic metadata path for DATABASE-FILE."
  (concat (expand-file-name database-file) ".owner"))

(defun e-runtime-store-ownership--lock-file (database-file)
  "Return Emacs' cross-process lock path for DATABASE-FILE."
  (make-lock-file-name (expand-file-name database-file)))

(defun e-runtime-store-ownership--bounded-string (value limit)
  "Return string VALUE limited to LIMIT characters, or nil."
  (and (stringp value)
       (substring value 0 (min (length value) limit))))

(defun e-runtime-store-ownership--runtime-id (runtime-id)
  "Return the bounded diagnostic form of RUNTIME-ID."
  (or (e-runtime-store-ownership--bounded-string
       runtime-id e-runtime-store-ownership-runtime-id-max-chars)
      (signal 'wrong-type-argument (list 'stringp runtime-id))))

(defun e-runtime-store-ownership--process-attributes (pid)
  "Return boundedly queried process attributes for live PID, or nil."
  (and (integerp pid) (> pid 0)
       (condition-case nil
           (process-attributes pid)
         (error nil))))

(defun e-runtime-store-ownership--process-start (attributes)
  "Return the process start identity recorded in ATTRIBUTES."
  (alist-get 'start attributes))

(defun e-runtime-store-ownership--process-arguments (attributes)
  "Return a bounded command representation from process ATTRIBUTES."
  (let ((arguments (alist-get 'args attributes)))
    (cond
     ((stringp arguments)
      (e-runtime-store-ownership--bounded-string arguments
                                                 e-runtime-store-ownership-metadata-max-bytes))
     ((listp arguments)
      (e-runtime-store-ownership--bounded-string
       (mapconcat (lambda (argument) (format "%s" argument)) arguments " ")
       e-runtime-store-ownership-metadata-max-bytes)))))

(defun e-runtime-store-ownership--worker-role (attributes)
  "Return the runtime-worker role identified by process ATTRIBUTES, or nil."
  (let ((arguments
         (e-runtime-store-ownership--process-arguments attributes)))
    (cond
     ((and arguments
           (string-match-p "e-runtime-store-offline-worker-main" arguments))
      'offline)
     ((and arguments
           (string-match-p "e-runtime-store-worker-main" arguments))
      'ordinary))))

(defun e-runtime-store-ownership--live-identity (pid attributes)
  "Return the bounded live identity for PID and its ATTRIBUTES."
  (list :pid pid
        :process-start
        (e-runtime-store-ownership--process-start attributes)
        :role (e-runtime-store-ownership--worker-role attributes)))

(defun e-runtime-store-ownership--metadata-summary (metadata)
  "Return a bounded diagnostic summary of ownership METADATA."
  (when (listp metadata)
    (list :runtime-id
          (e-runtime-store-ownership--bounded-string
           (plist-get metadata :runtime-id)
           e-runtime-store-ownership-runtime-id-max-chars)
          :pid (let ((pid (plist-get metadata :pid)))
                 (and (integerp pid) pid))
          :process-start (plist-get metadata :process-start)
          :role (let ((role (plist-get metadata :role)))
                  (and (memq role '(ordinary offline)) role)))))

(defun e-runtime-store-ownership--read-metadata (database-file)
  "Return bounded ownership metadata for DATABASE-FILE, or nil when invalid."
  (let ((metadata-file
         (e-runtime-store-ownership--metadata-file database-file)))
    (when-let* ((attributes (file-attributes metadata-file))
                (size (file-attribute-size attributes))
                ((<= size e-runtime-store-ownership-metadata-max-bytes)))
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents-literally metadata-file nil 0 size)
            (let ((read-eval nil)
                  metadata)
              (setq metadata (read (current-buffer)))
              (and (listp metadata) metadata)))
        (error nil)))))

(defun e-runtime-store-ownership--current-process-start ()
  "Return this process' start identity, or fail before making a claim."
  (let* ((pid (emacs-pid))
         (attributes (e-runtime-store-ownership--process-attributes pid))
         (start (and attributes
                     (e-runtime-store-ownership--process-start attributes))))
    (or start
        (signal 'e-runtime-store-ownership-error
                (list "Cannot establish runtime worker process-start identity"
                      :pid pid)))))

(defun e-runtime-store-ownership--metadata (runtime-id role)
  "Build bounded ownership metadata for RUNTIME-ID and ROLE."
  (unless (memq role '(ordinary offline))
    (signal 'wrong-type-argument (list '(member ordinary offline) role)))
  (list :runtime-id (e-runtime-store-ownership--runtime-id runtime-id)
        :pid (emacs-pid)
        :process-start (e-runtime-store-ownership--current-process-start)
        :role role))

(defun e-runtime-store-ownership--write-metadata (database-file metadata)
  "Atomically replace DATABASE-FILE's diagnostic METADATA while locked."
  (let* ((metadata-file
          (e-runtime-store-ownership--metadata-file database-file))
         (temporary (make-temp-file (concat metadata-file ".tmp-")))
         (coding-system-for-write 'utf-8-unix))
    (unwind-protect
        (progn
          (write-region (prin1-to-string metadata) nil temporary nil 'silent)
          (set-file-modes temporary #o600)
          (rename-file temporary metadata-file t)
          (set-file-modes metadata-file #o600)
          metadata)
      (when (file-exists-p temporary)
        (ignore-errors (delete-file temporary))))))

(defun e-runtime-store-ownership--parse-lock (database-file)
  "Return bounded identity from DATABASE-FILE's Emacs lock, or nil."
  (when-let* ((target
               (file-symlink-p
                (e-runtime-store-ownership--lock-file database-file)))
              ((stringp target)))
    (let ((target
           (e-runtime-store-ownership--bounded-string
            target e-runtime-store-ownership-metadata-max-bytes)))
      (when (string-match
             "\\`\\(?:[^@]+@\\)?\\(.+\\)\\.\\([0-9]+\\):\\([0-9]+\\)\\'"
             target)
        (list :host (match-string 1 target)
              :pid (string-to-number (match-string 2 target))
              :boot (string-to-number (match-string 3 target)))))))

(defun e-runtime-store-ownership--local-lock-p (lock)
  "Return non-nil when LOCK belongs to this host."
  (equal (plist-get lock :host) (system-name)))

(defun e-runtime-store-ownership--metadata-matches-live-p
    (metadata pid attributes)
  "Return non-nil when METADATA identifies live PID with ATTRIBUTES."
  (and (listp metadata)
       (or (not (integerp (plist-get metadata :pid)))
           (= (plist-get metadata :pid) pid))
       (or (not (plist-member metadata :process-start))
           (equal (plist-get metadata :process-start)
                  (e-runtime-store-ownership--process-start attributes)))
       (let ((metadata-role (plist-get metadata :role))
             (live-role (e-runtime-store-ownership--worker-role attributes)))
         (or (not (memq metadata-role '(ordinary offline)))
             (not live-role)
             (eq metadata-role live-role)))))

(defun e-runtime-store-ownership--identity-conflict-p (lock metadata)
  "Return non-nil when live LOCK conflicts with metadata or process identity."
  (when-let* ((pid (plist-get lock :pid))
              ((e-runtime-store-ownership--local-lock-p lock))
              (attributes
               (e-runtime-store-ownership--process-attributes pid)))
    (or (not (e-runtime-store-ownership--worker-role attributes))
        (and metadata
             (not (e-runtime-store-ownership--metadata-matches-live-p
                   metadata pid attributes))))))

(defun e-runtime-store-ownership--signal-contention (database-file)
  "Signal the typed bounded contention failure for DATABASE-FILE."
  (let* ((lock (e-runtime-store-ownership--parse-lock database-file))
         (metadata (e-runtime-store-ownership--read-metadata database-file))
         (pid (plist-get lock :pid))
         (attributes
          (and (e-runtime-store-ownership--local-lock-p lock)
               (e-runtime-store-ownership--process-attributes pid)))
         (properties
          (list :lock lock
                :metadata
                (e-runtime-store-ownership--metadata-summary metadata)
                :live (and attributes
                            (e-runtime-store-ownership--live-identity
                             pid attributes)))))
    (if (e-runtime-store-ownership--identity-conflict-p lock metadata)
        (signal 'e-runtime-store-owner-identity-conflict
                (cons "Runtime-store lock conflicts with a live process identity"
                      properties))
      (signal 'e-runtime-store-owner-active
              (cons "Runtime-store lock is held by a live owner" properties)))))

(defun e-runtime-store-ownership--acquire-lock (database-file)
  "Atomically acquire DATABASE-FILE's Emacs lock or signal typed contention."
  (let ((create-lockfiles t))
    (cl-letf (((symbol-function 'ask-user-about-lock)
               (lambda (file opponent)
                 (signal 'e-runtime-store-ownership--contention
                         (list file opponent)))))
      (condition-case err
          (progn
            (lock-file database-file)
            t)
        (e-runtime-store-ownership--contention
         (e-runtime-store-ownership--signal-contention database-file))
        (error (signal (car err) (cdr err)))))))

(defun e-runtime-store-ownership--legacy-live-owner-p (metadata)
  "Return non-nil when lockless METADATA still names a live runtime worker."
  (when-let* ((pid (plist-get metadata :pid))
              (attributes (e-runtime-store-ownership--process-attributes pid))
              (role (e-runtime-store-ownership--worker-role attributes)))
    (list :metadata (e-runtime-store-ownership--metadata-summary metadata)
          :live (e-runtime-store-ownership--live-identity pid attributes)
          :role role)))

(defun e-runtime-store-ownership-acquire (database-file runtime-id role)
  "Atomically claim DATABASE-FILE for RUNTIME-ID in ROLE before SQLite opens.

The adjacent metadata is written only after Emacs' file lock succeeds."
  (let* ((database-file (expand-file-name database-file))
         (metadata-file
          (e-runtime-store-ownership--metadata-file database-file))
         (metadata (e-runtime-store-ownership--metadata runtime-id role))
         (lock-held nil)
         claim)
    (e-runtime-store-ownership--acquire-lock database-file)
    (setq lock-held t)
    (unwind-protect
        (let ((existing (e-runtime-store-ownership--read-metadata database-file)))
          (when-let* ((legacy-live
                       (e-runtime-store-ownership--legacy-live-owner-p existing)))
            ;; A lockless legacy record is not authority, but a currently live
            ;; worker remains unsafe to overlap.  Leave its record intact.
            (signal 'e-runtime-store-owner-active
                    (list "Legacy runtime-store owner is still live"
                          :metadata (plist-get legacy-live :metadata)
                          :live (plist-get legacy-live :live))))
          (e-runtime-store-ownership--write-metadata database-file metadata)
          (setq claim
                (e-runtime-store-ownership-claim--create
                 :database-file database-file :metadata-file metadata-file
                 :runtime-id (plist-get metadata :runtime-id)
                 :role role :pid (plist-get metadata :pid)
                 :process-start (plist-get metadata :process-start)
                 :metadata metadata)
                lock-held nil)
          claim)
      (when lock-held
        (unlock-file database-file)))))

(defun e-runtime-store-ownership-release (claim)
  "Release CLAIM after its SQLite connection has closed.

Metadata is deleted only while the current process still owns the Emacs lock;
`unlock-file' then preserves a replacement lock held by another process."
  (when claim
    (let ((database-file
           (e-runtime-store-ownership-claim--database-file claim)))
      (unwind-protect
          (when (and (eq (file-locked-p database-file) t)
                     (equal
                      (e-runtime-store-ownership--read-metadata database-file)
                      (e-runtime-store-ownership-claim--metadata claim)))
            (let ((metadata-file
                   (e-runtime-store-ownership-claim--metadata-file claim)))
              (when (file-exists-p metadata-file)
                (delete-file metadata-file))))
        (unlock-file database-file)))))

(provide 'e-runtime-store-ownership)

;;; e-runtime-store-ownership.el ends here
