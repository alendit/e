;;; e-session-cutover-prepare-test.el --- Offline session cutover preparation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'json)
(require 'e-session-persistence)

(defun e-session-cutover-prepare-test--script ()
  "Return the bundled offline preparation script."
  (expand-file-name
   "e-session-cutover-prepare.mjs"
   (file-name-directory (locate-library "e-session-persistence"))))

(defun e-session-cutover-prepare-test--write (file text)
  "Write TEXT to FILE, creating its parent directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun e-session-cutover-prepare-test--run
    (store manifest snapshot &optional output-buffer)
  "Run preparation for STORE, MANIFEST, and SNAPSHOT.
Send combined process output to OUTPUT-BUFFER when supplied."
  (let ((buffer (or output-buffer (generate-new-buffer " *cutover-prepare*"))))
    (unwind-protect
        (process-file
         e-session-persistence-node-executable nil buffer nil
         (e-session-cutover-prepare-test--script)
         "--store" store "--manifest" manifest "--snapshot" snapshot)
      (unless output-buffer (kill-buffer buffer)))))

(defun e-session-cutover-prepare-test--last-record (journal)
  "Return the last JSON object from JOURNAL as a plist."
  (with-temp-buffer
    (insert-file-contents journal)
    (goto-char (point-max))
    (forward-line -1)
    (when (eolp) (forward-line -1))
    (json-parse-string
     (buffer-substring-no-properties (line-beginning-position) (line-end-position))
     :object-type 'plist :array-type 'list :null-object nil)))

(ert-deftest e-session-cutover-prepare-test-snapshots-and-installs-current-rows ()
  "Preparation preserves the old tree and derives exact ACL/counter rows."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((root (make-temp-file "e-cutover-prepare-" t))
         (store (expand-file-name "store" root))
         (snapshot (expand-file-name "snapshot" root))
         (manifest (expand-file-name "manifest.json" root))
         (journal (expand-file-name "sessions/session.jsonl" store)))
    (unwind-protect
        (progn
          (e-session-cutover-prepare-test--write
           journal
           (concat
            "{\"type\":\"session\",\"session-id\":\"session\",\"timestamp\":\"2026-08-01T00:00:00Z\"}\n"
            "{\"type\":\"message\",\"session-id\":\"session\",\"message\":{\"role\":\"assistant\",\"board-output-sequence\":5}}\n"
            "{\"type\":\"activity-event\",\"session-id\":\"session\",\"board-activity-sequence\":8}\n"))
          (e-session-cutover-prepare-test--write
           manifest
           (concat
            "{\"schema-version\":1,\"session-store-id\":\"store\",\"sessions\":["
            "{\"session-id\":\"session\",\"controller\":\"owner\","
            "\"discover-principals\":[\"reader\"],\"resume-principals\":[\"runner\"]}]}\n"))
          (should (= (e-session-cutover-prepare-test--run
                      store manifest snapshot)
                     0))
          (let ((record (e-session-cutover-prepare-test--last-record journal)))
            (should (equal (plist-get record :type) "board-session-state"))
            (should (equal (plist-get record :state) "dormant"))
            (should (= (plist-get record :board-output-sequence) 5))
            (should (= (plist-get record :board-activity-sequence) 8))
            (let ((access (plist-get record :access-record)))
              (should (equal (plist-get access :controller) "owner"))
              (should (= (plist-get access :version) 0))
              (should (equal (plist-get access :discover-principals)
                             '("reader")))
              (should (equal (plist-get access :resume-principals)
                             '("runner")))))
          (with-temp-buffer
            (insert-file-contents
             (expand-file-name "sessions/session.jsonl" snapshot))
            (should-not (search-forward "board-session-state" nil t)))
          (let* ((catalog
                  (with-temp-buffer
                    (insert-file-contents (expand-file-name "index.json" store))
                    (json-parse-buffer :object-type 'plist :array-type 'list
                                       :null-object nil)))
                 (row (car catalog)))
            (should (equal (plist-get row :id) "session"))
            (should (equal (plist-get row :state) "dormant"))
            (should (= (plist-get row :board-output-sequence) 5))
            (should (= (plist-get row :board-activity-sequence) 8))))
      (delete-directory root t))))

(ert-deftest e-session-cutover-prepare-test-invalid-manifest-leaves-store-untouched ()
  "Missing policy fails before snapshot creation or journal mutation."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((root (make-temp-file "e-cutover-invalid-" t))
         (store (expand-file-name "store" root))
         (snapshot (expand-file-name "snapshot" root))
         (manifest (expand-file-name "manifest.json" root))
         (journal (expand-file-name "sessions/session.jsonl" store))
         (original
          "{\"type\":\"session\",\"session-id\":\"session\",\"timestamp\":\"2026-08-01T00:00:00Z\"}\n"))
    (unwind-protect
        (progn
          (e-session-cutover-prepare-test--write journal original)
          (e-session-cutover-prepare-test--write
           manifest
           "{\"schema-version\":1,\"session-store-id\":\"store\",\"sessions\":[]}\n")
          (should-not (= (e-session-cutover-prepare-test--run
                          store manifest snapshot)
                         0))
          (should-not (file-exists-p snapshot))
          (with-temp-buffer
            (insert-file-contents journal)
            (should (equal (buffer-string) original))))
      (delete-directory root t))))

(ert-deftest e-session-cutover-prepare-test-refuses-an-already-prepared-store ()
  "The one-shot command never appends a second current-state record."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((root (make-temp-file "e-cutover-repeat-" t))
         (store (expand-file-name "store" root))
         (first-snapshot (expand-file-name "snapshot-1" root))
         (second-snapshot (expand-file-name "snapshot-2" root))
         (manifest (expand-file-name "manifest.json" root))
         (journal (expand-file-name "sessions/session.jsonl" store)))
    (unwind-protect
        (progn
          (e-session-cutover-prepare-test--write
           journal
           "{\"type\":\"session\",\"session-id\":\"session\",\"timestamp\":\"2026-08-01T00:00:00Z\"}\n")
          (e-session-cutover-prepare-test--write
           manifest
           (concat
            "{\"schema-version\":1,\"session-store-id\":\"store\",\"sessions\":["
            "{\"session-id\":\"session\",\"controller\":\"owner\","
            "\"discover-principals\":[],\"resume-principals\":[]}]}\n"))
          (should (= (e-session-cutover-prepare-test--run
                      store manifest first-snapshot)
                     0))
          (should-not (= (e-session-cutover-prepare-test--run
                          store manifest second-snapshot)
                         0))
          (should-not (file-exists-p second-snapshot))
          (with-temp-buffer
            (insert-file-contents journal)
            (goto-char (point-min))
            (should (= (how-many "board-session-state" (point-min) (point-max))
                       1))))
      (delete-directory root t))))

(ert-deftest e-session-cutover-prepare-test-preserves-an-existing-lock ()
  "A competing preparation lock is a hard fence owned by its creator."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((root (make-temp-file "e-cutover-lock-" t))
         (store (expand-file-name "store" root))
         (snapshot (expand-file-name "snapshot" root))
         (manifest (expand-file-name "manifest.json" root))
         (journal (expand-file-name "sessions/session.jsonl" store))
         (lock (expand-file-name ".store.board-cutover-preparation.lock" root)))
    (unwind-protect
        (progn
          (e-session-cutover-prepare-test--write
           journal
           "{\"type\":\"session\",\"session-id\":\"session\",\"timestamp\":\"2026-08-01T00:00:00Z\"}\n")
          (e-session-cutover-prepare-test--write
           manifest
           (concat
            "{\"schema-version\":1,\"session-store-id\":\"store\",\"sessions\":["
            "{\"session-id\":\"session\",\"controller\":\"owner\","
            "\"discover-principals\":[],\"resume-principals\":[]}]}\n"))
          (e-session-cutover-prepare-test--write lock "owned elsewhere\n")
          (should-not (= (e-session-cutover-prepare-test--run
                          store manifest snapshot)
                         0))
          (should-not (file-exists-p snapshot))
          (with-temp-buffer
            (insert-file-contents lock)
            (should (equal (buffer-string) "owned elsewhere\n"))))
      (delete-directory root t))))

(provide 'e-session-cutover-prepare-test)

;;; e-session-cutover-prepare-test.el ends here
