;;; e-session-tmp-resources-test.el --- Tests for tmp:// session resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for session-scoped temporary resources.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-request)
(require 'e-resources)
(require 'e-tools)
(require 'e-session-tmp-resources)

(defun e-session-tmp-test--fake-executable (directory name body)
  "Create executable NAME in DIRECTORY with shell BODY."
  (let ((file (expand-file-name name directory)))
    (write-region (concat "#!/bin/sh\n" body) nil file nil 'silent)
    (set-file-modes file #o755)
    file))

(ert-deftest e-session-tmp-test-helper-writes-and-resource-read-recovers ()
  "The helper writes session tmp content and returns a readable tmp:// URI."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))
         (uri (e-session-tmp-write
               harness
               "session-1"
               "tool-results/turn-1/call-1.txt"
               "full output")))
    (should (equal uri "tmp://tool-results/turn-1/call-1.txt"))
    (should (equal (e-resources-read
                    (e-harness-resources harness "session-1" "turn-1")
                    uri
                    nil)
                   "full output"))))

(ert-deftest e-session-tmp-test-write-does-not-prompt-for-coding ()
  "Writing eight-bit content never invokes the coding-system selector.
Regression: tmp:// writes left `coding-system-for-write' unbound, so bytes the
buffer's coding could not encode triggered `select-safe-coding-system' and
blocked on the interactive coding-system picker."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))
         ;; Eight-bit content the prefer-utf-8 default coding cannot encode;
         ;; an unbound `coding-system-for-write' would route it through the
         ;; selector.
         (content (decode-coding-string (unibyte-string 200 201 202) 'binary))
         (select-safe-coding-system-function
          (lambda (&rest _)
            (error "select-safe-coding-system must not run"))))
    ;; Succeeds (returns the URI) without the selector ever running.
    (should (equal (e-session-tmp-write harness "session-1" "raw.bin" content)
                   "tmp://raw.bin"))))

(ert-deftest e-session-tmp-test-resource-write-creates-parents ()
  "Model-facing tmp:// writes create missing parents inside the session root."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))
         (resources (e-harness-resources harness "session-1" "turn-1")))
    (should (equal (e-resources-write resources
                                      "tmp://notes/new.txt"
                                      "created")
                   "tmp://notes/new.txt"))
    (should (equal (e-resources-read resources "tmp://notes/new.txt" nil)
                   "created"))))

(ert-deftest e-session-tmp-test-cleanup-session-deletes-owned-root ()
  "Session tmp cleanup deletes the session root and forgets the owner mapping."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))
         (first-uri (e-session-tmp-write harness "session-1" "out.txt" "one"))
         (first-root (e-session-tmp-directory harness "session-1")))
    (should (equal first-uri "tmp://out.txt"))
    (should (file-exists-p (expand-file-name "out.txt" first-root)))
    (should (equal (e-session-tmp-cleanup-session harness "session-1")
                   first-root))
    (should-not (file-exists-p first-root))
    (let ((second-root (e-session-tmp-directory harness "session-1")))
      (should-not (equal second-root first-root))
      (should (file-directory-p second-root))
      (e-session-tmp-cleanup-session harness "session-1"))))

(ert-deftest e-session-tmp-test-cleanup-harness-deletes-all-session-roots ()
  "Harness tmp cleanup deletes every session root owned by the harness."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create)))))
    (e-session-tmp-write harness "session-1" "one.txt" "one")
    (e-session-tmp-write harness "session-2" "two.txt" "two")
    (let ((root-1 (e-session-tmp-directory harness "session-1"))
          (root-2 (e-session-tmp-directory harness "session-2")))
      (should (file-directory-p root-1))
      (should (file-directory-p root-2))
      (should (eq (e-session-tmp-cleanup-harness harness) harness))
      (should-not (file-exists-p root-1))
      (should-not (file-exists-p root-2)))))

(ert-deftest e-session-tmp-test-cleanup-expired-deletes-stale-roots ()
  "Expired tmp cleanup deletes only roots older than the configured age."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))
         (now 1000.0))
    (e-session-tmp-write harness "old-session" "old.txt" "old")
    (e-session-tmp-write harness "fresh-session" "fresh.txt" "fresh")
    (let ((old-root (e-session-tmp-directory harness "old-session"))
          (fresh-root (e-session-tmp-directory harness "fresh-session")))
      (set-file-times old-root (seconds-to-time 900))
      (set-file-times fresh-root (seconds-to-time 995))
      (should (equal (e-session-tmp-cleanup-expired 50 now)
                     (list old-root)))
      (should-not (file-exists-p old-root))
      (should (file-directory-p fresh-root))
      (let ((new-old-root (e-session-tmp-directory harness "old-session")))
        (should-not (equal new-old-root old-root))
        (should (file-directory-p new-old-root)))
      (e-session-tmp-cleanup-harness harness))))

(ert-deftest e-session-tmp-test-harness-reset-cleans-session-root ()
  "The tmp resource capability cleans session-owned files on harness reset."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create)))))
    (e-harness-create-session harness :id "session-1")
    (e-session-tmp-write harness "session-1" "tool-results/out.txt" "large")
    (let ((root (e-session-tmp-directory harness "session-1")))
      (should (file-exists-p (expand-file-name "tool-results/out.txt" root)))
      (e-harness-reset harness "session-1")
      (should-not (file-exists-p root)))))

(ert-deftest e-session-tmp-test-resource-edit-is-strict ()
  "tmp:// edits apply exact replacements to existing files only."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))
         (resources (e-harness-resources harness "session-1" "turn-1")))
    (e-resources-write resources "tmp://notes/edit.txt" "alpha beta alpha")
    (should (equal (e-resources-edit
                    resources
                    "tmp://notes/edit.txt"
                    '((:oldText "beta" :newText "BETA")))
                   "tmp://notes/edit.txt"))
    (should (equal (e-resources-read resources "tmp://notes/edit.txt" nil)
                   "alpha BETA alpha"))
    (should-error (e-resources-edit
                   resources
                   "tmp://notes/edit.txt"
                   '((:oldText "missing" :newText "x")))
                  :type 'e-session-tmp-resources-edit-mismatch)
    (should-error (e-resources-edit
                   resources
                   "tmp://notes/missing.txt"
                   '((:oldText "x" :newText "y")))
                  :type 'file-missing)))

(ert-deftest e-session-tmp-test-glob-and-search-resources ()
  "tmp:// glob and search inspect files inside the session tmp root."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))
         (resources (e-harness-resources harness "session-1" "turn-1")))
    (e-resources-write resources "tmp://notes/one.txt" "Alpha needle\n")
    (e-resources-write resources "tmp://notes/two.log" "other\n")
    (e-resources-write resources "tmp://notes/literal[abc].txt" "literal\n")
    (e-resources-write resources "tmp://tool-results/out.txt" "needle again\n")
    (e-resources-write resources "tmp://a/one.txt" "nested\n")
    (e-resources-write resources "tmp://b/two.txt" "nested\n")
    (e-resources-write resources "tmp://z.txt" "root\n")
    (let* ((content (e-resources-glob resources "tmp://notes" "*.txt" 5))
           (items (append (plist-get content :resources) nil)))
      (should (equal (mapcar (lambda (item) (plist-get item :uri)) items)
                     '("tmp://notes/literal[abc].txt"
                       "tmp://notes/one.txt")))
      (should (equal (plist-get content :truncated) nil)))
    (should
     (equal (e-resources-glob resources "tmp://notes/one.txt" "*.txt" 5)
            '(:resources [(:uri "tmp://notes/one.txt"
                            :name "one.txt"
                            :kind file
                            :metadata (:bytes 13))]
              :truncated nil)))
    (should
     (equal (e-resources-glob resources "tmp://notes/one.txt" "*.md" 5)
            '(:resources [] :truncated nil)))
    (should
     (equal (e-resources-glob
             resources
             "tmp://notes"
             "literal[abc].txt"
             5)
            '(:resources [(:uri "tmp://notes/literal[abc].txt"
                            :name "literal[abc].txt"
                            :kind file
                            :metadata (:bytes 8))]
              :truncated nil)))
    (should
     (equal (e-resources-glob resources "tmp://" "*.txt" 1)
            '(:resources [(:uri "tmp://z.txt"
                            :name "z.txt"
                            :kind file
                            :metadata (:bytes 5))]
              :truncated nil)))
    (should
     (equal (e-resources-search
             resources
             "tmp://"
             "needle"
             '(:glob "tool-results/*.txt" :limit 5))
            '(:matches [(:uri "tmp://tool-results/out.txt"
                          :line 1
                          :column 1
                          :text "needle again")]
              :truncated nil)))
    (should
	     (equal (e-resources-search
	             resources
	             "tmp://"
	             "missing"
	             nil)
	            '(:matches [] :truncated nil)))))

(ert-deftest e-session-tmp-test-glob-start-returns-before-fd-exits ()
  "tmp:// glob starts a cancellable fd process without waiting for completion."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((bin-dir (make-temp-file "e-session-tmp-bin-" t))
         (_fd (e-session-tmp-test--fake-executable
               bin-dir
               "fd"
               "sleep 5
printf '%s\\n' notes/delayed.txt
"))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))
         (resources (e-harness-resources harness "session-1" "turn-1"))
         (tools (e-tools-registry-create))
         result
         failure
         request)
    (unwind-protect
        (let ((exec-path (cons bin-dir exec-path)))
          (e-harness--register-resource-tools tools resources)
          (e-resources-write resources "tmp://notes/delayed.txt" "text")
          (e-request-with-blocking-primitive-guard
            (e-request-with-hot-path 'tmp-glob
              (setq request
                    (e-tools-start
                     tools
                     '(:id "call-1"
                       :name "glob"
                       :arguments (:uri "tmp://"
                                   :pattern "**/*.txt"
                                   :limit 5))
                     :context '(:interactive t)
	                     :on-done (lambda (value)
	                                (setq result value))
	                     :on-error (lambda (err)
	                                 (setq failure err))))))
          (should (e-tools-request-p request))
          (should (process-live-p
                   (plist-get (e-tools-request-metadata request) :process)))
          (should-not result)
          (should-not failure)
          (should (e-tools-cancel-request request)))
      (delete-directory bin-dir t))))

(ert-deftest e-session-tmp-test-discovery-rejects-unsafe-paths ()
  "tmp:// glob/search roots stay inside the session tmp root."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))
         (resources (e-harness-resources harness "session-1" "turn-1")))
    (should-error (e-resources-glob resources "tmp://../escape" "*.txt" 5)
                  :type 'e-session-tmp-resources-invalid-path)
    (should-error (e-resources-search
                   resources
                   "tmp://../escape"
                   "needle"
                   nil)
                  :type 'e-session-tmp-resources-invalid-path)))

(ert-deftest e-session-tmp-test-discovery-reports-missing-commands ()
  "tmp:// glob/search report missing fd and rg commands clearly."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))
         (resources (e-harness-resources harness "session-1" "turn-1"))
         (original-executable-find (symbol-function 'executable-find)))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (command)
                 (unless (member command '("fd" "fdfind" "rg"))
                   (funcall original-executable-find command)))))
      (should-error (e-resources-glob resources "tmp://" "*.txt" 5)
                    :type 'e-session-tmp-resources-missing-command)
      (should-error (e-resources-glob resources "tmp://" "foo**bar" 5)
                    :type 'e-resource-pattern-invalid)
      (should-error (e-resources-search
                     resources
                     "tmp://"
                     "needle"
                     '(:glob "foo**bar"))
                    :type 'e-resource-pattern-invalid)
      (should-error (e-resources-search
                     resources
                     "tmp://"
                     "needle"
                     nil)
                    :type 'e-session-tmp-resources-missing-command))))

(ert-deftest e-session-tmp-test-rejects-unsafe-paths ()
  "tmp:// resource paths stay inside the session root."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))
         (resources (e-harness-resources harness "session-1" "turn-1")))
    (should-error (e-session-tmp-write harness "session-1" "../escape.txt" "x")
                  :type 'e-session-tmp-resources-invalid-path)
    (should-error (e-resources-read resources "tmp://../escape.txt" nil)
                  :type 'e-session-tmp-resources-invalid-path)
    (should-error (e-resources-write resources "tmp:///absolute.txt" "x")
                  :type 'e-session-tmp-resources-invalid-path)
    (should-error (e-resources-write resources "tmp://bad\0name.txt" "x")
                  :type 'e-session-tmp-resources-invalid-path)))

(ert-deftest e-session-tmp-test-read-supports-line-ranges ()
  "tmp:// reads support the first-slice line range contract."
  (should (require 'e-session-tmp-resources nil t))
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))
         (resources (e-harness-resources harness "session-1" "turn-1")))
    (e-resources-write resources "tmp://notes/range.txt" "one\ntwo\nthree\n")
    (should (equal (e-resources-read
                    resources
                    "tmp://notes/range.txt"
                    '(:unit "line" :start 2 :end 3))
                   "two\nthree\n"))))

(provide 'e-session-tmp-resources-test)

;;; e-session-tmp-resources-test.el ends here
