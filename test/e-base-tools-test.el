;;; e-base-tools-test.el --- Tests for base filesystem and shell tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for Pi-like base tools.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-base)
(require 'e-base-tools)
(require 'e-harness)
(require 'e-harness-base)
(require 'e-resources)
(require 'e-tools)
(require 'seq)

(defun e-base-tools-test--resource-tools (directory &optional read-only)
  "Return resource-backed tools rooted at DIRECTORY.
When READ-ONLY is non-nil, file resources only support reads."
  (let ((resources (e-resources-registry-create))
        (tools (e-tools-registry-create)))
    (if read-only
        (e-base-tools-register-file-read-resource resources directory)
      (e-base-tools-register-file-resource resources directory))
    (e-harness--register-resource-tools tools resources)
    tools))

(defun e-base-tools-test--execute (registry name arguments)
  "Execute NAME with ARGUMENTS against REGISTRY."
  (e-tools-execute
   registry
   (list :id "call-1" :name name :arguments arguments)))

(defun e-base-tools-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(ert-deftest e-base-tools-test-read-file-full-and-range ()
  "The read tool reads full and ranged text files."
  (let* ((directory (make-temp-file "e-base-read-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-base-tools-test--resource-tools directory)))
    (unwind-protect
        (progn
          (write-region "one\ntwo\nthree\n" nil file nil 'silent)
          (should
           (equal (plist-get
                   (e-base-tools-test--execute
                    registry "read" '(:uri "file://sample.txt"))
                   :content)
                  "one\ntwo\nthree\n"))
          (should
           (equal (plist-get
                   (e-base-tools-test--execute
                    registry
                    "read"
                    '(:uri "file://sample.txt"
                      :range (:unit "line" :start 2 :end 2)))
                   :content)
                  "two\n\n[2 more lines in file. Use offset=3 to continue.]")))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-read-file-reports-resource-usage-metadata ()
  "Resource-backed read results report the URI and operation they used."
  (let* ((directory (make-temp-file "e-base-read-usage-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-base-tools-test--resource-tools directory)))
    (unwind-protect
        (progn
          (write-region "one\n" nil file nil 'silent)
          (let* ((result (e-base-tools-test--execute
                          registry "read" '(:uri "file://sample.txt")))
                 (metadata (plist-get result :metadata)))
            (should
             (equal (plist-get metadata :tool-usage)
                    '((:kind resource-usage
                       :tool "read"
                       :resources ((:uri "file://sample.txt"
                                    :operation read))))))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-glob-file-resources ()
  "The glob tool lists file:// resources under roots and patterns."
  (let* ((directory (make-temp-file "e-base-glob-" t))
         (nested (expand-file-name "lisp/core" directory))
         (registry (e-base-tools-test--resource-tools directory)))
    (unwind-protect
        (progn
          (make-directory nested t)
          (write-region "alpha\n" nil
                        (expand-file-name "README.md" directory)
                        nil 'silent)
          (write-region "beta\n" nil
                        (expand-file-name "e-resources.el" nested)
                        nil 'silent)
          (write-region "gamma\n" nil
                        (expand-file-name "notes.txt" nested)
                        nil 'silent)
          (write-region "delta\n" nil
                        (expand-file-name "more.txt" nested)
                        nil 'silent)
          (write-region "epsilon\n" nil
                        (expand-file-name "literal[abc].txt" nested)
                        nil 'silent)
          (make-directory (expand-file-name "a" directory) t)
          (make-directory (expand-file-name "b" directory) t)
          (write-region "root\n" nil
                        (expand-file-name "z.txt" directory)
                        nil 'silent)
          (write-region "nested\n" nil
                        (expand-file-name "a/one.txt" directory)
                        nil 'silent)
          (write-region "nested\n" nil
                        (expand-file-name "b/two.txt" directory)
                        nil 'silent)
          (should
           (equal (plist-get
                   (e-base-tools-test--execute
                    registry
                    "glob"
                    '(:uri "file://lisp" :pattern "**/*.el" :limit 5))
                   :content)
                  '(:resources [(:uri "file://lisp/core/e-resources.el"
                                  :name "core/e-resources.el"
                                 :kind file
                                 :metadata (:bytes 5))]
                    :truncated nil)))
          (should
           (equal (plist-get
                   (e-base-tools-test--execute
                    registry
                    "glob"
                    '(:uri "file://lisp/core/notes.txt" :pattern "*.txt" :limit 5))
                   :content)
                  '(:resources [(:uri "file://lisp/core/notes.txt"
                                  :name "notes.txt"
                                  :kind file
                                  :metadata (:bytes 6))]
                    :truncated nil)))
          (should
           (equal (plist-get
                   (e-base-tools-test--execute
                    registry
                    "glob"
                    '(:uri "file://lisp/core/notes.txt" :pattern "*.md" :limit 5))
                   :content)
                  '(:resources [] :truncated nil)))
          (should
           (equal (plist-get
                   (e-base-tools-test--execute
                    registry
                    "glob"
                    '(:uri "file://lisp/core"
                      :pattern "literal[abc].txt"
                      :limit 5))
                   :content)
                  '(:resources [(:uri "file://lisp/core/literal[abc].txt"
                                  :name "literal[abc].txt"
                                  :kind file
                                  :metadata (:bytes 8))]
                    :truncated nil)))
          (should
           (equal (plist-get
                   (e-base-tools-test--execute
                    registry
                    "glob"
                    '(:uri "file://" :pattern "*.txt" :limit 1))
                   :content)
                  '(:resources [(:uri "file://z.txt"
                                  :name "z.txt"
                                  :kind file
                                  :metadata (:bytes 5))]
                    :truncated nil)))
          (let* ((content (plist-get
                           (e-base-tools-test--execute
                            registry
                            "glob"
                            '(:uri "file://" :pattern "**/*.txt" :limit 1))
                           :content))
                 (resources (plist-get content :resources)))
            (should (equal (length resources) 1))
            (should (member (plist-get (elt resources 0) :uri)
                            '("file://lisp/core/more.txt"
                              "file://lisp/core/notes.txt"
                              "file://lisp/core/literal[abc].txt"
                              "file://a/one.txt"
                              "file://b/two.txt"
                              "file://z.txt")))
            (should (equal (plist-get content :truncated) t))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-search-file-resources ()
  "The search tool searches file:// resources with rg-backed options."
  (let* ((directory (make-temp-file "e-base-search-" t))
         (nested (expand-file-name "src" directory))
         (registry (e-base-tools-test--resource-tools directory)))
    (unwind-protect
        (progn
          (make-directory nested t)
          (write-region "Alpha needle\nbeta\n" nil
                        (expand-file-name "one.el" nested)
                        nil 'silent)
          (write-region "alpha NEEDLE\nneedle again\n" nil
                        (expand-file-name "two.txt" nested)
                        nil 'silent)
          (should
           (equal (plist-get
                   (e-base-tools-test--execute
                    registry
                    "search"
                    '(:uri "file://src"
                      :query "needle"
                      :glob "*.el"
                      :limit 5))
                   :content)
                  '(:matches [(:uri "file://src/one.el"
                                :line 1
                                :column 7
                                :text "Alpha needle")]
                    :truncated nil)))
          (should
           (equal (plist-get
                   (e-base-tools-test--execute
                    registry
                    "search"
                    '(:uri "file://src"
                      :query "needle again"
                      :glob "*.txt"
                      :case-sensitive t
                      :limit 5))
                   :content)
                  '(:matches [(:uri "file://src/two.txt"
                                :line 2
                                :column 1
                                :text "needle again")]
                    :truncated nil)))
          (should
           (equal (plist-get
                   (e-base-tools-test--execute
                    registry
                    "search"
                    '(:uri "file://src"
                      :query "missing"))
                   :content)
                  '(:matches [] :truncated nil))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-file-discovery-rejects-outside-root ()
  "File glob/search reject roots outside the configured workspace root."
  (let* ((directory (make-temp-file "e-base-discovery-root-" t))
         (registry (e-base-tools-test--resource-tools directory)))
    (unwind-protect
        (let ((glob (e-base-tools-test--execute
                     registry "glob" '(:uri "file://../outside.txt")))
              (search (e-base-tools-test--execute
                       registry
                       "search"
                       '(:uri "file://../outside.txt" :query "needle"))))
          (should (equal (plist-get glob :status) 'error))
          (should (string-match-p "escapes workspace root"
                                  (plist-get glob :content)))
          (should (equal (plist-get search :status) 'error))
          (should (string-match-p "escapes workspace root"
                                  (plist-get search :content))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-file-discovery-reports-missing-commands ()
  "File glob/search report missing fd and rg commands clearly."
  (let* ((directory (make-temp-file "e-base-discovery-missing-" t))
         (registry (e-base-tools-test--resource-tools directory))
         (resources (e-resources-registry-create))
         (original-executable-find (symbol-function 'executable-find)))
    (unwind-protect
        (progn
          (e-base-tools-register-file-resource resources directory)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (command)
                       (unless (member command '("fd" "fdfind" "rg"))
                         (funcall original-executable-find command)))))
            (should-error
             (e-resources-glob resources "file://" "foo**bar" 5)
             :type 'e-resource-pattern-invalid)
            (should-error
             (e-resources-search
              resources
              "file://"
              "needle"
              '(:glob "foo**bar"))
             :type 'e-resource-pattern-invalid)
          (let ((glob (e-base-tools-test--execute
                       registry "glob" '(:uri "file://")))
                (search (e-base-tools-test--execute
                         registry
                         "search"
                         '(:uri "file://" :query "needle"))))
            (should (equal (plist-get glob :status) 'error))
            (should (string-match-p "Missing executable: fd"
                                    (plist-get glob :content)))
            (should (equal (plist-get search :status) 'error))
            (should (string-match-p "Missing executable: rg"
                                    (plist-get search :content))))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-read-file-errors-for-missing-and-binary ()
  "The read tool fails clearly for missing and binary files."
  (let* ((directory (make-temp-file "e-base-read-errors-" t))
         (binary-file (expand-file-name "image.png" directory))
         (registry (e-base-tools-test--resource-tools directory t)))
    (unwind-protect
        (progn
          (write-region (unibyte-string 137 80 78 71 13 10 26 10 0)
                        nil binary-file nil 'silent)
          (let ((missing (e-base-tools-test--execute
                          registry "read" '(:uri "file://missing.txt")))
                (binary (e-base-tools-test--execute
                         registry "read" '(:uri "file://image.png"))))
            (should (equal (plist-get missing :status) 'error))
            (should (string-match-p "File is not readable"
                                    (plist-get missing :content)))
            (should (equal (plist-get binary :status) 'error))
            (should (string-match-p "text-only"
                                    (plist-get binary :content)))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-read-file-truncates-with-continuation ()
  "The read tool truncates large text files with a continuation hint."
  (let* ((directory (make-temp-file "e-base-read-truncate-" t))
         (file (expand-file-name "large.txt" directory))
         (registry (e-base-tools-test--resource-tools directory)))
    (unwind-protect
        (progn
          (write-region
           (mapconcat (lambda (index) (format "line-%04d" index))
                      (number-sequence 1 2105)
                      "\n")
           nil file nil 'silent)
          (let ((content (plist-get
                          (e-base-tools-test--execute
                           registry "read" '(:uri "file://large.txt"))
                          :content)))
            (should (string-match-p "line-0001" content))
            (should-not (string-match-p "line-2105" content))
            (should (string-match-p
                     "\\[Showing lines 1-[0-9]+ of 2105\\. Use offset=[0-9]+ to continue\\.\\]"
                     content))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-write-file-creates-parents-and-overwrites ()
  "The write tool creates parent directories and overwrites files."
  (let* ((directory (make-temp-file "e-base-write-" t))
         (target (expand-file-name "nested/file.txt" directory))
         (registry (e-base-tools-test--resource-tools directory)))
    (unwind-protect
        (progn
          (let ((result (e-base-tools-test--execute
                         registry
                         "write"
                         '(:uri "file://nested/file.txt" :content "new text"))))
            (should (equal (plist-get result :status) 'ok))
            (should (string-match-p "Successfully wrote 8 bytes"
                                    (plist-get result :content)))
            (should (equal (with-temp-buffer
                             (insert-file-contents target)
                             (buffer-string))
                           "new text")))
          (e-base-tools-test--execute
           registry
           "write"
           '(:uri "file://nested/file.txt" :content "replacement"))
          (should (equal (with-temp-buffer
                           (insert-file-contents target)
                           (buffer-string))
                         "replacement")))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-resolves-paths-in-secondary-root ()
  "File resources accept absolute paths into a configured secondary root."
  (let* ((primary (make-temp-file "e-base-primary-" t))
         (secondary (make-temp-file "e-base-secondary-" t))
         (outside (make-temp-file "e-base-outside-" t))
         (sec-file (expand-file-name "note.txt" secondary))
         (out-file (expand-file-name "note.txt" outside))
         (registry (e-base-tools-test--resource-tools
                    (list primary secondary))))
    (unwind-protect
        (progn
          (write-region "secondary" nil sec-file nil 'silent)
          (write-region "outside" nil out-file nil 'silent)
          (let ((ok (e-base-tools-test--execute
                     registry "read"
                     (list :uri (concat "file://" sec-file)))))
            (should (equal (plist-get ok :status) 'ok))
            (should (string-match-p "secondary" (plist-get ok :content))))
          (let ((rejected (e-base-tools-test--execute
                           registry "read"
                           (list :uri (concat "file://" out-file)))))
            (should (equal (plist-get rejected :status) 'error))
            (should (string-match-p "escapes workspace root"
                                    (plist-get rejected :content)))))
      (delete-directory primary t)
      (delete-directory secondary t)
      (delete-directory outside t))))

(ert-deftest e-base-tools-test-discovers-secondary-root-resources ()
  "Discovery and sync-status tools accept configured secondary roots."
  (let* ((primary (make-temp-file "e-base-primary-" t))
         (secondary (make-temp-file "e-base-secondary-" t))
         (outside (make-temp-file "e-base-outside-" t))
         (sec-dir (expand-file-name "src" secondary))
         (sec-file (expand-file-name "two.txt" sec-dir))
         (out-file (expand-file-name "bad.txt" outside))
         (registry (e-base-tools-test--resource-tools
                    (list primary secondary)))
         (status-registry (e-tools-registry-create)))
    (unwind-protect
        (progn
          (make-directory sec-dir t)
          (write-region "needle\n" nil sec-file nil 'silent)
          (write-region "outside\n" nil out-file nil 'silent)
          (e-base-tools-register-resource-sync-status
           status-registry
           (list primary secondary))
          (let* ((glob (e-base-tools-test--execute
                        registry
                        "glob"
                        (list :uri (concat "file://" sec-dir)
                              :pattern "*.txt"
                              :limit 5)))
                 (content (plist-get glob :content))
                 (resources (plist-get content :resources)))
            (should (equal (plist-get glob :status) 'ok))
            (should (equal (length resources) 1))
            (should (equal (plist-get (elt resources 0) :name) "two.txt"))
            (should (string-match-p "two\\.txt\\'"
                                    (plist-get (elt resources 0) :uri)))
            (should-not (plist-get content :truncated)))
          (let* ((search (e-base-tools-test--execute
                          registry
                          "search"
                          (list :uri (concat "file://" sec-dir)
                                :query "needle"
                                :limit 5)))
                 (content (plist-get search :content))
                 (matches (plist-get content :matches)))
            (should (equal (plist-get search :status) 'ok))
            (should (equal (length matches) 1))
            (should (string-match-p "two\\.txt\\'"
                                    (plist-get (elt matches 0) :uri)))
            (should (equal (plist-get (elt matches 0) :text) "needle"))
            (should-not (plist-get content :truncated)))
          (let ((status (e-base-tools-test--execute
                         status-registry
                         "resource_sync_status"
                         (list :uri (concat "file://" sec-file)))))
            (should (equal (plist-get status :status) 'ok))
            (should (eq (plist-get (plist-get status :content) :status)
                        'coherent))
            (should (plist-get (plist-get status :content) :disk-exists)))
          (dolist (call (list (list registry
                                    "glob"
                                    (list :uri (concat "file://" out-file)
                                          :pattern "*.txt"
                                          :limit 5))
                              (list registry
                                    "search"
                                    (list :uri (concat "file://" outside)
                                          :query "outside"
                                          :limit 5))
                              (list status-registry
                                    "resource_sync_status"
                                    (list :uri (concat "file://" out-file)))))
            (let ((result (e-base-tools-test--execute
                           (nth 0 call)
                           (nth 1 call)
                           (nth 2 call))))
              (should (equal (plist-get result :status) 'error))
              (should (string-match-p "escapes workspace root"
                                      (plist-get result :content))))))
      (delete-directory primary t)
      (delete-directory secondary t)
      (delete-directory outside t))))

(ert-deftest e-base-tools-test-write-file-rejects-paths-outside-root ()
  "The write tool does not create files outside its configured root."
  (let* ((parent (make-temp-file "e-base-write-root-" t))
         (directory (expand-file-name "workspace" parent))
         (outside (expand-file-name "outside.txt" parent))
         (registry nil))
    (unwind-protect
        (progn
          (make-directory directory)
          (setq registry (e-base-tools-test--resource-tools directory))
          (let ((result (e-base-tools-test--execute
                         registry
                         "write"
                         '(:uri "file://../outside.txt" :content "escape"))))
            (should (equal (plist-get result :status) 'error))
            (should (string-match-p "escapes workspace root"
                                    (plist-get result :content)))
            (should-not (file-exists-p outside))))
      (delete-directory parent t))))

(ert-deftest e-base-tools-test-write-file-reports-invalid-intermediate-path ()
  "The write tool fails clearly when a parent segment is a file."
  (let* ((directory (make-temp-file "e-base-write-invalid-parent-" t))
         (parent-as-file (expand-file-name "nested" directory))
         (target (expand-file-name "nested/file.txt" directory))
         (registry (e-base-tools-test--resource-tools directory)))
    (unwind-protect
        (progn
          (write-region "not a directory" nil parent-as-file nil 'silent)
          (let ((result (e-base-tools-test--execute
                         registry
                         "write"
                         '(:uri "file://nested/file.txt" :content "new"))))
            (should (equal (plist-get result :status) 'error))
            (should (string-match-p "Not a directory\\|File exists"
                                    (plist-get result :content)))
            (should-not (file-exists-p target))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-edit-file-applies-disjoint-edits-and-preserves-crlf ()
  "The edit tool applies multiple exact edits and preserves CRLF endings."
  (let* ((directory (make-temp-file "e-base-edit-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-base-tools-test--resource-tools directory)))
    (unwind-protect
        (progn
          (write-region "alpha\r\nbeta\r\ngamma\r\n" nil file nil 'silent)
          (let ((result (e-base-tools-test--execute
                         registry
                         "edit"
                         '(:uri "file://sample.txt"
                           :edits ((:oldText "alpha" :newText "ALPHA")
                                   (:oldText "gamma" :newText "GAMMA"))))))
            (should (equal (plist-get result :status) 'ok))
            (should (equal (plist-get (plist-get result :content) :replacements)
                           2))
            (should (string-match-p "^-alpha" (plist-get (plist-get result :content) :diff)))
            (should (equal (with-temp-buffer
                             (insert-file-contents-literally file)
                             (buffer-string))
                           "ALPHA\r\nbeta\r\nGAMMA\r\n"))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-edit-file-decodes-utf-8-text ()
  "The edit tool decodes UTF-8 text before applying replacements."
  (let* ((directory (make-temp-file "e-base-edit-utf8-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-base-tools-test--resource-tools directory)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8))
            (write-region "before “smart” after\n" nil file nil 'silent))
          (let ((result (e-base-tools-test--execute
                         registry
                         "edit"
                         '(:uri "file://sample.txt"
                           :edits ((:oldText "after" :newText "done"))))))
            (should (equal (plist-get result :status) 'ok))
            (should (equal (with-temp-buffer
                             (let ((coding-system-for-read 'utf-8))
                               (insert-file-contents file))
                             (buffer-string))
                           "before “smart” done\n"))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-edit-file-rejects-invalid-replacements ()
  "The edit tool rejects missing, duplicate, empty, overlapping, and no-op edits."
  (let* ((directory (make-temp-file "e-base-edit-errors-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-base-tools-test--resource-tools directory)))
    (unwind-protect
        (progn
          (write-region "alpha beta beta gamma" nil file nil 'silent)
          (dolist (case '(("missing" (:uri "file://sample.txt"
                                     :edits ((:oldText "missing" :newText "x"))))
                          ("unique" (:uri "file://sample.txt"
                                    :edits ((:oldText "beta" :newText "x"))))
                          ("empty" (:uri "file://sample.txt"
                                   :edits ((:oldText "" :newText "x"))))
                          ("overlap" (:uri "file://sample.txt"
                                     :edits ((:oldText "alpha beta" :newText "x")
                                             (:oldText "beta beta" :newText "y"))))
                          ("No changes" (:uri "file://sample.txt"
                                        :edits ((:oldText "alpha" :newText "alpha"))))))
            (let ((result (e-base-tools-test--execute registry "edit" (cadr case))))
              (should (equal (plist-get result :status) 'error))
              (should (string-match-p (car case) (plist-get result :content))))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-edit-file-does-not-create-missing-targets ()
  "The edit tool stays strict and does not create missing file paths."
  (let* ((directory (make-temp-file "e-base-edit-missing-" t))
         (parent (expand-file-name "nested" directory))
         (target (expand-file-name "nested/file.txt" directory))
         (registry (e-base-tools-test--resource-tools directory)))
    (unwind-protect
        (let ((result (e-base-tools-test--execute
                       registry
                       "edit"
                       '(:uri "file://nested/file.txt"
                         :edits ((:oldText "old" :newText "new"))))))
          (should (equal (plist-get result :status) 'error))
          (should (string-match-p "File is not readable"
                                  (plist-get result :content)))
          (should-not (file-exists-p parent))
          (should-not (file-exists-p target)))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-file-live-buffers-reports-linked-state ()
  "File/buffer discovery reports linked buffer metadata."
  (let* ((directory (make-temp-file "e-base-live-buffers-" t))
         (file (expand-file-name "sample.txt" directory))
         buffer)
    (unwind-protect
        (progn
          (write-region "disk" nil file nil 'silent)
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert " changed"))
          (let* ((state (e-base-tools-file-link-state file))
                 (buffers (plist-get state :buffers))
                 (first (car buffers)))
            (should (equal (plist-get state :file) (file-truename file)))
            (should (equal (plist-get first :name) (buffer-name buffer)))
            (should (plist-get first :modified))
            (should-not (plist-get first :visible))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-read-file-prefers-unsaved-live-buffer ()
  "file:// reads return live buffer contents when the file is open in Emacs."
  (let* ((directory (make-temp-file "e-base-read-live-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-base-tools-test--resource-tools directory))
         buffer)
    (unwind-protect
        (progn
          (write-region "disk" nil file nil 'silent)
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (erase-buffer)
            (insert "live unsaved"))
          (should (equal (plist-get
                          (e-base-tools-test--execute
                           registry "read" '(:uri "file://sample.txt"))
                          :content)
                         "live unsaved")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-write-file-through-live-buffer-never-prompts-coding ()
  "Writing through a live buffer saves UTF-8 without a coding-system prompt.
Regression: `save-buffer' would call `select-safe-coding-system' and prompt
the user when the buffer's coding could not encode the new content."
  (let* ((directory (make-temp-file "e-base-write-coding-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-base-tools-test--resource-tools directory))
         buffer)
    (unwind-protect
        (progn
          ;; Existing ASCII file, opened with an ASCII coding that cannot
          ;; encode the non-ASCII content the agent is about to write.
          (write-region "plain" nil file nil 'silent)
          (let ((coding-system-for-read 'us-ascii))
            (setq buffer (find-file-noselect file)))
          (with-current-buffer buffer
            (setq buffer-file-coding-system 'us-ascii))
          ;; Fail loudly if anything tries to interactively choose a coding.
          (let ((select-safe-coding-system-function
                 (lambda (&rest _)
                   (error "coding-system prompt should not happen"))))
            (let ((result (e-base-tools-test--execute
                           registry "write"
                           '(:uri "file://sample.txt"
                             :content "café — naïve ☕"))))
              (should (memq (plist-get result :status) '(ok nil)))))
          ;; Content landed on disk as UTF-8.
          (should (equal (with-temp-buffer
                           (let ((coding-system-for-read 'utf-8))
                             (insert-file-contents file))
                           (buffer-string))
                         "café — naïve ☕")))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-write-file-conflicts-with-modified-live-buffer ()
  "Direct file writes fail when a visiting buffer has unsaved changes."
  (let* ((directory (make-temp-file "e-base-write-conflict-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-base-tools-test--resource-tools directory))
         buffer)
    (unwind-protect
        (progn
          (write-region "disk" nil file nil 'silent)
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert " unsaved"))
          (let ((result (e-base-tools-test--execute
                         registry "write"
                         '(:uri "file://sample.txt" :content "new"))))
            (should (equal (plist-get result :status) 'error))
            (should (string-match-p "unsaved changes"
                                    (plist-get result :content)))
            (should (equal (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))
                           "disk"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-write-file-conflicts-with-stale-live-buffer ()
  "Direct file writes fail when a visiting buffer is stale on disk."
  (let* ((directory (make-temp-file "e-base-write-stale-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-base-tools-test--resource-tools directory))
         (status-registry (e-tools-registry-create))
         buffer)
    (unwind-protect
        (progn
          (e-base-tools-register-resource-sync-status status-registry directory)
          (write-region "disk" nil file nil 'silent)
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (write-region "disk changed" nil file nil 'silent)
          (let ((status (e-base-tools-test--execute
                         status-registry "resource_sync_status"
                         '(:uri "file://sample.txt"))))
            (should (eq (plist-get (plist-get status :content) :status)
                        'stale)))
          (let ((result (e-base-tools-test--execute
                         registry "write"
                         '(:uri "file://sample.txt" :content "new"))))
            (should (equal (plist-get result :status) 'error))
            (should (string-match-p "stale"
                                    (plist-get result :content)))
            (should (equal (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))
                           "disk changed"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-edit-file-conflicts-with-stale-live-buffer ()
  "Direct file edits fail when a visiting buffer is stale on disk."
  (let* ((directory (make-temp-file "e-base-edit-stale-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-base-tools-test--resource-tools directory))
         buffer)
    (unwind-protect
        (progn
          (write-region "disk" nil file nil 'silent)
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (write-region "disk changed" nil file nil 'silent)
          (let ((result (e-base-tools-test--execute
                         registry "edit"
                         '(:uri "file://sample.txt"
                           :edits ((:oldText "disk"
                                    :newText "new"))))))
            (should (equal (plist-get result :status) 'error))
            (should (string-match-p "stale"
                                    (plist-get result :content)))
            (should (equal (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))
                           "disk changed"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-write-file-routes-through-unmodified-live-buffer ()
  "Direct file writes update and save a linked unmodified buffer."
  (let* ((directory (make-temp-file "e-base-write-live-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-base-tools-test--resource-tools directory))
         buffer)
    (unwind-protect
        (progn
          (write-region "old" nil file nil 'silent)
          (setq buffer (find-file-noselect file))
          (should-not (with-current-buffer buffer (buffer-modified-p)))
          (let ((result (e-base-tools-test--execute
                         registry "write"
                         '(:uri "file://sample.txt" :content "new"))))
            (should (equal (plist-get result :status) 'ok))
            (should (string-match-p "through live buffer"
                                    (plist-get result :content))))
          (should (equal (with-current-buffer buffer (buffer-string)) "new"))
          (should-not (with-current-buffer buffer (buffer-modified-p)))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "new")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-edit-through-live-buffer-preserves-overlays ()
  "Editing through a live buffer keeps overlays anchored to surviving text.
Regression: erase-buffer+insert collapsed every overlay to position 1, which
corrupted minor-mode state persisted from overlays (e.g. Simply Annotate
annotation regions).  `replace-buffer-contents' must relocate the overlay with
its text instead."
  (let* ((directory (make-temp-file "e-base-edit-overlay-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-base-tools-test--resource-tools directory))
         buffer overlay)
    (unwind-protect
        (progn
          (write-region "line one\nANCHOR\nline three\n" nil file nil 'silent)
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "ANCHOR")
            (setq overlay (make-overlay (match-beginning 0) (match-end 0)))
            (overlay-put overlay 'test-marker t))
          (e-base-tools-test--execute
           registry "edit"
           '(:uri "file://sample.txt"
             :edits [(:oldText "line one\n" :newText "preamble\nline one\n")]))
          (should (overlay-buffer overlay))
          (should (equal (with-current-buffer buffer
                           (buffer-substring-no-properties
                            (overlay-start overlay) (overlay-end overlay)))
                         "ANCHOR"))
          (should (> (overlay-start overlay) 1)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-resource-sync-status-reports-needs-save-and-stale ()
  "The resource_sync_status tool reports linked buffer coherence states."
  (let* ((directory (make-temp-file "e-base-sync-status-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-tools-registry-create))
         buffer)
    (unwind-protect
        (progn
          (write-region "disk" nil file nil 'silent)
          (e-base-tools-register-resource-sync-status registry directory)
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert " unsaved"))
          (let ((result (e-base-tools-test--execute
                         registry "resource_sync_status"
                         '(:uri "file://sample.txt"))))
            (should (equal (plist-get result :status) 'ok))
            (should (eq (plist-get (plist-get result :content) :status)
                        'needs-save)))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (write-region "disk changed" nil file nil 'silent)
          (let ((result (e-base-tools-test--execute
                         registry "resource_sync_status"
                         '(:uri "file://sample.txt"))))
            (should (eq (plist-get (plist-get result :content) :status)
                        'stale))
            (should (plist-get (plist-get result :content) :disk-exists))
            (should-not (plist-member (plist-get result :content)
                                      :disk-readable))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-bash-captures-output-and-errors ()
  "The bash tool captures stdout/stderr and reports nonzero exits."
  (let* ((directory (make-temp-file "e-base-bash-" t))
         (registry (e-tools-registry-create)))
    (unwind-protect
        (progn
          (e-base-tools-register-bash registry directory)
          (let ((result (e-base-tools-test--execute
                         registry
                         "bash"
                         '(:command "printf out; printf err >&2"))))
            (should (equal (plist-get result :status) 'ok))
            (should (equal (plist-get result :content) "outerr")))
          (let ((result (e-base-tools-test--execute
                         registry
                         "bash"
                         '(:command "printf fail; exit 7"))))
            (should (equal (plist-get result :status) 'error))
            (should (string-match-p "fail" (plist-get result :content)))
            (should (string-match-p "Command exited with code 7"
                                    (plist-get result :content)))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-bash-output-with-raw-bytes-does-not-prompt ()
  "Bash output containing eight-bit bytes never invokes the coding selector.
Regression: streaming bash output to its log file via `write-region' left
`coding-system-for-write' unbound, so undecodable bytes triggered
`select-safe-coding-system' and blocked on the interactive coding-system
picker."
  (let* ((directory (make-temp-file "e-base-bash-raw-" t))
         (registry (e-tools-registry-create))
         (select-safe-coding-system-function
          (lambda (&rest _)
            (error "select-safe-coding-system must not run"))))
    (unwind-protect
        (progn
          (e-base-tools-register-bash registry directory)
          (let ((result (e-base-tools-test--execute
                         registry
                         "bash"
                         ;; \xC0\xC1 are invalid UTF-8 lead bytes.
                         '(:command "printf '\\300\\301'"))))
            (should (equal (plist-get result :status) 'ok))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-bash-accepts-optional-resource-usage-metadata ()
  "Free tools can report high-value affected resources when supplied."
  (let* ((directory (make-temp-file "e-base-bash-usage-" t))
         (registry (e-tools-registry-create)))
    (unwind-protect
        (progn
          (e-base-tools-register-bash registry directory)
          (let* ((result (e-base-tools-test--execute
                          registry
                          "bash"
                          '(:command "printf ok"
                            :resource_usage
                            (:resources ((:uri "file://notes.org"
                                          :operation read))
                             :summary "checked notes"))))
                 (metadata (plist-get result :metadata)))
            (should (equal (plist-get result :status) 'ok))
            (should
             (equal (plist-get metadata :tool-usage)
                    '((:kind resource-usage
                       :tool "bash"
                       :resources ((:uri "file://notes.org"
                                    :operation read))
                       :summary "checked notes"))))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-bash-times-out-and-truncates-output ()
  "The bash tool honors timeouts and saves truncated output."
  (let* ((directory (make-temp-file "e-base-bash-limits-" t))
         (registry (e-tools-registry-create)))
    (unwind-protect
        (progn
          (e-base-tools-register-bash registry directory)
          (let ((timeout-result (e-base-tools-test--execute
                                 registry
                                 "bash"
                                 '(:command "sleep 2" :timeout 0.1))))
            (should (equal (plist-get timeout-result :status) 'error))
            (should (string-match-p "Command timed out after 0.1 seconds"
                                    (plist-get timeout-result :content))))
          (let* ((result (e-base-tools-test--execute
                          registry
                          "bash"
                          '(:command "yes line | head -n 2105")))
                 (content (plist-get result :content))
                 (metadata (plist-get result :metadata)))
            (should (equal (plist-get result :status) 'ok))
            (should (string-match-p "Full output:" content))
            (should (string-prefix-p "line\nline\n" content))
            (should (plist-get metadata :truncated))
            (should (equal (plist-get metadata :original-lines) 2105))
            (should (file-readable-p
                     (plist-get metadata :full-output-path)))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-bash-timeout-schema-documents-deadline ()
  "The bash timeout parameter documents units and process-kill behavior."
  (let ((registry (e-tools-registry-create)))
    (e-base-tools-register-bash registry default-directory)
    (let* ((definition (seq-find
                        (lambda (tool)
                          (equal (plist-get tool :name) "bash"))
                        (e-tools-definitions registry)))
           (properties (plist-get (plist-get definition :parameters)
                                  :properties))
           (timeout (plist-get properties :timeout))
           (description (plist-get timeout :description)))
      (should (stringp description))
      (should (string-match-p "seconds" description))
      (should (string-match-p "kills the process" description))
      (should (string-match-p "modest" description)))))

(ert-deftest e-base-tools-test-bash-streams-large-output-to-session-tmp ()
  "The bash start path writes full large output directly to session tmp."
  (let* ((directory (make-temp-file "e-base-bash-session-tmp-" t))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers
                   (list (e-harness-base-layer-create)
                         (e-base-layer-create directory))))
         (result nil)
         request)
    (unwind-protect
        (let ((e-tool-output-truncation-max-bytes 1000)
              (e-tool-output-truncation-max-lines 2))
          (setq request
                (e-tool-lifecycle-start-call
                 (e-harness-tool-lifecycle harness "session-1" "turn-1")
                 '(:id "call-1"
                   :name "bash"
                   :arguments (:command "printf 'one\ntwo\nthree\nfour\n'"))
                 :on-done (lambda (value) (setq result value))))
          (should (e-tools-request-p request))
          (should (plist-get (e-tools-request-metadata request) :output-file))
          (should-not (plist-get (e-tools-request-metadata request) :buffer))
          (should (e-base-tools-test--wait-until
                   (lambda () result)
                   1.0))
          (let* ((metadata (plist-get result :metadata))
                 (uri (plist-get metadata :tmp-uri)))
            (should (equal (plist-get result :status) 'ok))
            (should (plist-get metadata :truncated))
            (should (equal uri "tmp://tool-results/turn-1/bash-call-1.txt"))
            (should (string-prefix-p "one\ntwo\n" (plist-get result :content)))
            (should (string-match-p (regexp-quote uri)
                                    (plist-get result :content)))
            (should (equal (e-resources-read
                            (e-harness-resources harness "session-1" "turn-1")
                            uri
                            nil)
                           "one\ntwo\nthree\nfour\n"))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-bash-starts-asynchronously ()
  "The native bash tool start path returns before command completion."
  (let* ((directory (make-temp-file "e-base-bash-async-" t))
         (registry (e-tools-registry-create))
         result)
    (unwind-protect
        (progn
          (e-base-tools-register-bash registry directory)
          (let ((request
                 (e-tools-start
                  registry
                  '(:id "call-1"
                    :name "bash"
                    :arguments (:command "sleep 0.1; printf done"))
                  :on-done (lambda (value) (setq result value)))))
            (should (e-tools-request-p request))
            (should (null result))
            (should (e-base-tools-test--wait-until
                     (lambda () result)
                     1.0))
            (should (equal (plist-get result :status) 'ok))
            (should (equal (plist-get result :content) "done"))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-bash-timeout-kills-process-asynchronously ()
  "The native bash timeout path kills the process and reports a tool error."
  (let* ((directory (make-temp-file "e-base-bash-timeout-" t))
         (registry (e-tools-registry-create))
         result
         request)
    (unwind-protect
        (progn
          (e-base-tools-register-bash registry directory)
          (setq request
                (e-tools-start
                 registry
                 '(:id "call-1"
                   :name "bash"
                   :arguments (:command "sleep 2" :timeout 0.1))
                 :on-done (lambda (value) (setq result value))))
          (should (e-tools-request-p request))
          (should (e-base-tools-test--wait-until
                   (lambda () result)
                   1.0))
          (should (equal (plist-get result :status) 'error))
          (should (string-match-p "Command timed out after 0.1 seconds"
                                  (plist-get result :content)))
          (when-let ((process (plist-get (e-tools-request-metadata request)
                                         :process)))
            (should-not (process-live-p process))))
      (delete-directory directory t))))

(provide 'e-base-tools-test)

;;; e-base-tools-test.el ends here
