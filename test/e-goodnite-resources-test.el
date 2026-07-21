;;; e-goodnite-resources-test.el --- Tests for goodnite:// resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the read-only goodnite:// knowledge scheme (daydream).

;;; Code:

(require 'ert)
(require 'seq)
(require 'e-operations)
(require 'e-resources)
(require 'e-goodnite-resources)
(require 'e-goodnite)

(defvar e-goodnite-resources-test--home nil
  "Temporary goodnite home used by the current test.")

(defun e-goodnite-resources-test--write (relative content)
  "Write CONTENT to RELATIVE under the test goodnite home."
  (let ((path (expand-file-name relative e-goodnite-resources-test--home)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content))
    path))

(defun e-goodnite-resources-test--seed ()
  "Populate the temporary goodnite home with representative artifacts."
  (e-goodnite-resources-test--write
   "candidates/resolving-rebase-conflicts/SKILL.md"
   (concat
    "---\n"
    "name: resolving-rebase-conflicts\n"
    "description: Use when a git rebase stops on a conflict and you must\n"
    "  finish the rebase without losing work.\n"
    "status: candidate\n"
    "cluster_id: c_0009\n"
    "---\n\n"
    "# Resolving rebase conflicts\n\n"
    "Inspect the conflict, resolve each hunk, then continue the rebase.\n"))
  (e-goodnite-resources-test--write
   "candidates/promoted-thing/SKILL.md"
   (concat
    "---\n"
    "name: promoted-thing\n"
    "description: A reviewed workflow.\n"
    "status: promoted\n"
    "---\n\n"
    "# Promoted thing\n\nDo the reviewed thing.\n"))
  (e-goodnite-resources-test--write
   "gotchas/c_0042.md"
   (concat
    "---\n"
    "cluster_id: c_0042\n"
    "nearest_skill: resolving-rebase-conflicts\n"
    "success_rate: 0.2\n"
    "---\n\n"
    "Do not force-push over a shared branch while a rebase is half-applied.\n"))
  (e-goodnite-resources-test--write
   "facts/grimoire.md"
   (concat
    "---\n"
    "project_root: /Users/x/projects/org/grimoire/\n"
    "marker_count: 3\n"
    "---\n\n"
    "- Read out-of-workspace files with bash cat.\n")))

(defmacro e-goodnite-resources-test--with-home (&rest body)
  "Run BODY with a freshly seeded temporary goodnite home."
  (declare (indent 0) (debug t))
  `(let* ((e-goodnite-resources-test--home
           (make-temp-file "e-goodnite-test" t))
          (e-goodnite-home e-goodnite-resources-test--home))
     (unwind-protect
         (progn (e-goodnite-resources-test--seed) ,@body)
       (delete-directory e-goodnite-resources-test--home t))))

(defun e-goodnite-resources-test--glob (uri &optional pattern)
  "Glob URI with optional PATTERN in the test home."
  (e-goodnite-resources--glob
   (e-resources-parse-uri uri) pattern nil nil nil nil nil nil nil nil))

(defun e-goodnite-resources-test--names (glob-result)
  "Return the entry names in GLOB-RESULT."
  (mapcar (lambda (entry) (plist-get entry :name))
          (append (plist-get glob-result :resources) nil)))

(ert-deftest e-goodnite-resources-test-root-glob-lists-consumer-types ()
  "goodnite:// lists exactly the three consumer knowledge types."
  (e-goodnite-resources-test--with-home
    (should (equal (e-goodnite-resources-test--names
                    (e-goodnite-resources-test--glob "goodnite://"))
                   '("workflows" "pitfalls" "conventions")))))

(ert-deftest e-goodnite-resources-test-type-glob-maps-artifacts ()
  "Each artifact family surfaces under its consumer type."
  (e-goodnite-resources-test--with-home
    (should (equal (sort (e-goodnite-resources-test--names
                          (e-goodnite-resources-test--glob "goodnite://workflows/"))
                         #'string<)
                   '("promoted-thing" "resolving-rebase-conflicts")))
    (should (equal (e-goodnite-resources-test--names
                    (e-goodnite-resources-test--glob "goodnite://pitfalls/"))
                   '("c_0042")))
    (should (equal (e-goodnite-resources-test--names
                    (e-goodnite-resources-test--glob "goodnite://conventions/"))
                   '("grimoire")))))

(ert-deftest e-goodnite-resources-test-glob-stub-carries-consumer-fields ()
  "A workflow stub carries a multi-line when-to-use and a confidence hint."
  (e-goodnite-resources-test--with-home
    (let* ((entries (append (plist-get
                             (e-goodnite-resources-test--glob "goodnite://workflows/")
                             :resources)
                            nil))
           (entry (seq-find (lambda (e)
                              (equal (plist-get e :name) "resolving-rebase-conflicts"))
                            entries))
           (meta (plist-get entry :metadata)))
      (should (string-match-p "finish the rebase without losing work"
                              (plist-get meta :when-to-use)))
      (should (equal (plist-get meta :confidence) "mined (unreviewed)")))))

(ert-deftest e-goodnite-resources-test-glob-unknown-type-signals ()
  "Globbing an unknown consumer type is an error."
  (e-goodnite-resources-test--with-home
    (should-error (e-goodnite-resources-test--glob "goodnite://nonsense/")
                  :type 'e-goodnite-resources-unknown-type)))

(ert-deftest e-goodnite-resources-test-glob-pattern-filters ()
  "A glob pattern filters entries by name and metadata."
  (e-goodnite-resources-test--with-home
    (should (equal (e-goodnite-resources-test--names
                    (e-goodnite-resources-test--glob "goodnite://workflows/" "*rebase*"))
                   '("resolving-rebase-conflicts")))))

(ert-deftest e-goodnite-resources-test-confidence-reflects-review ()
  "A promoted artifact reads as established, an unreviewed one as mined."
  (e-goodnite-resources-test--with-home
    (let ((entries (append (plist-get
                            (e-goodnite-resources-test--glob "goodnite://workflows/")
                            :resources)
                           nil)))
      (should (equal (plist-get (plist-get
                                 (seq-find
                                  (lambda (e) (equal (plist-get e :name) "promoted-thing"))
                                  entries)
                                 :metadata)
                                :confidence)
                     "established")))))

(ert-deftest e-goodnite-resources-test-read-renders-consumer-header ()
  "Reading a leaf drops goodnite frontmatter and adds a consumer header."
  (e-goodnite-resources-test--with-home
    (let ((body (e-goodnite-resources--read
                 (e-resources-parse-uri "goodnite://workflows/resolving-rebase-conflicts")
                 nil)))
      (should (string-match-p "type: workflow" body))
      (should (string-match-p "confidence: mined (unreviewed)" body))
      (should (string-match-p "# Resolving rebase conflicts" body))
      (should-not (string-match-p "cluster_id" body)))))

(ert-deftest e-goodnite-resources-test-read-pitfall-scope-nearest-skill ()
  "A pitfall exposes its nearest-skill pointer as scope, not raw frontmatter."
  (e-goodnite-resources-test--with-home
    (let ((body (e-goodnite-resources--read
                 (e-resources-parse-uri "goodnite://pitfalls/c_0042") nil)))
      (should (string-match-p "type: pitfall" body))
      (should (string-match-p "scope: near resolving-rebase-conflicts" body))
      (should-not (string-match-p "success_rate" body)))))

(ert-deftest e-goodnite-resources-test-read-non-leaf-signals ()
  "Reading a type root rather than a leaf is an error."
  (e-goodnite-resources-test--with-home
    (should-error (e-goodnite-resources--read
                   (e-resources-parse-uri "goodnite://workflows/") nil)
                  :type 'e-goodnite-resources-invalid-uri)))

(ert-deftest e-goodnite-resources-test-read-unknown-entry-signals ()
  "Reading a missing entry is an error."
  (e-goodnite-resources-test--with-home
    (should-error (e-goodnite-resources--read
                   (e-resources-parse-uri "goodnite://workflows/absent") nil)
                  :type 'e-goodnite-resources-unknown-entry)))

(ert-deftest e-goodnite-resources-test-read-line-range ()
  "A line range narrows the rendered body."
  (e-goodnite-resources-test--with-home
    (let ((head (e-goodnite-resources--read
                 (e-resources-parse-uri "goodnite://workflows/resolving-rebase-conflicts")
                 '(:unit "line" :start 1 :end 1))))
      (should (equal (string-trim head) "# resolving-rebase-conflicts")))))

(ert-deftest e-goodnite-resources-test-search-ranks-by-relevance ()
  "Search over all knowledge ranks the on-topic workflow first."
  (e-goodnite-resources-test--with-home
    (let* ((e-goodnite-search-semantic nil)
           (result (e-goodnite-resources--search
                    (e-resources-parse-uri "goodnite://") "rebase conflict"
                    '(:limit 5)))
           (matches (append (plist-get result :matches) nil)))
      (should matches)
      (should (equal (plist-get (car matches) :uri)
                     "goodnite://workflows/resolving-rebase-conflicts")))))

(ert-deftest e-goodnite-resources-test-search-scopes-to-type ()
  "Search scoped to one type only returns entries of that type."
  (e-goodnite-resources-test--with-home
    (let* ((e-goodnite-search-semantic nil)
           (result (e-goodnite-resources--search
                    (e-resources-parse-uri "goodnite://conventions/") "bash cat"
                    '(:limit 5)))
           (matches (append (plist-get result :matches) nil)))
      (should matches)
      (should (seq-every-p
               (lambda (m) (string-prefix-p "goodnite://conventions/"
                                            (plist-get m :uri)))
               matches)))))

(ert-deftest e-goodnite-resources-test-search-semantic-program ()
  "When a semantic program is configured, its ranked matches pass through."
  (e-goodnite-resources-test--with-home
    (let* ((script (expand-file-name "fake-goodnite"
                                     e-goodnite-resources-test--home))
           (payload (concat
                     "{\"matches\":[{\"uri\":\"goodnite://pitfalls/c_0042\","
                     "\"type\":\"pitfalls\",\"slug\":\"c_0042\","
                     "\"title\":\"c_0042\",\"when_to_use\":\"a pitfall\","
                     "\"confidence\":\"mined (unreviewed)\",\"text\":\"a pitfall\","
                     "\"score\":91000,\"rank\":1}],"
                     "\"truncated\":false,\"indexed\":true}")))
      (with-temp-file script
        (insert "#!/bin/sh\n")
        (insert "echo diagnostic on stderr 1>&2\n")
        (insert (format "echo '%s'\n" payload)))
      (set-file-modes script #o755)
      (let* ((e-goodnite-search-program script)
             (e-goodnite-search-semantic t)
             (result (e-goodnite-resources--search
                      (e-resources-parse-uri "goodnite://") "anything"
                      '(:limit 5)))
             (matches (append (plist-get result :matches) nil)))
        (should (= (length matches) 1))
        (should (equal (plist-get (car matches) :uri)
                       "goodnite://pitfalls/c_0042"))
        (should (equal (plist-get (car matches) :score) 91000))))))

(ert-deftest e-goodnite-resources-test-search-semantic-scope-filters ()
  "A type-scoped semantic search drops out-of-scope program matches."
  (e-goodnite-resources-test--with-home
    (let* ((script (expand-file-name "fake-goodnite"
                                     e-goodnite-resources-test--home))
           (payload (concat
                     "{\"matches\":["
                     "{\"uri\":\"goodnite://workflows/w\",\"type\":\"workflows\","
                     "\"slug\":\"w\",\"title\":\"w\",\"when_to_use\":\"x\","
                     "\"confidence\":\"mined (unreviewed)\",\"text\":\"x\","
                     "\"score\":90000,\"rank\":1},"
                     "{\"uri\":\"goodnite://pitfalls/p\",\"type\":\"pitfalls\","
                     "\"slug\":\"p\",\"title\":\"p\",\"when_to_use\":\"y\","
                     "\"confidence\":\"mined (unreviewed)\",\"text\":\"y\","
                     "\"score\":80000,\"rank\":2}],"
                     "\"truncated\":false,\"indexed\":true}")))
      (with-temp-file script
        (insert "#!/bin/sh\n")
        (insert (format "echo '%s'\n" payload)))
      (set-file-modes script #o755)
      (let* ((e-goodnite-search-program script)
             (e-goodnite-search-semantic t)
             (result (e-goodnite-resources--search
                      (e-resources-parse-uri "goodnite://pitfalls/") "y"
                      '(:limit 5)))
             (matches (append (plist-get result :matches) nil)))
        (should (= (length matches) 1))
        (should (equal (plist-get (car matches) :uri) "goodnite://pitfalls/p"))))))

(ert-deftest e-goodnite-resources-test-search-falls-back-to-lexical ()
  "An unindexed program result falls back to lexical search over entries."
  (e-goodnite-resources-test--with-home
    (let* ((script (expand-file-name "fake-goodnite"
                                     e-goodnite-resources-test--home)))
      (with-temp-file script
        (insert "#!/bin/sh\n")
        (insert "echo '{\"matches\":[],\"truncated\":false,\"indexed\":false}'\n"))
      (set-file-modes script #o755)
      (let* ((e-goodnite-search-program script)
             (e-goodnite-search-semantic t)
             (result (e-goodnite-resources--search
                      (e-resources-parse-uri "goodnite://") "rebase conflict"
                      '(:limit 5)))
             (matches (append (plist-get result :matches) nil)))
        (should matches)
        (should (equal (plist-get (car matches) :uri)
                       "goodnite://workflows/resolving-rebase-conflicts"))))))

(ert-deftest e-goodnite-resources-test-search-lexical-when-disabled ()
  "With semantic disabled, search is lexical even if a program exists."
  (e-goodnite-resources-test--with-home
    (let ((e-goodnite-search-semantic nil))
      (let* ((result (e-goodnite-resources--search
                      (e-resources-parse-uri "goodnite://") "rebase conflict"
                      '(:limit 5)))
             (matches (append (plist-get result :matches) nil)))
        (should matches)
        (should (equal (plist-get (car matches) :uri)
                       "goodnite://workflows/resolving-rebase-conflicts"))))))

(ert-deftest e-goodnite-resources-test-empty-home-globs-clean ()
  "An absent artifact family globs to nothing rather than erroring."
  (let* ((e-goodnite-resources-test--home (make-temp-file "e-goodnite-empty" t))
         (e-goodnite-home e-goodnite-resources-test--home))
    (unwind-protect
        (should (null (e-goodnite-resources-test--names
                       (e-goodnite-resources-test--glob "goodnite://workflows/"))))
      (delete-directory e-goodnite-resources-test--home t))))

(ert-deftest e-goodnite-resources-test-capability-and-layer-shape ()
  "The goodnite capability registers goodnite:// and carries the skill preamble."
  (let ((cap (e-goodnite-capability-create))
        (layer (e-goodnite-layer-create)))
    (should (eq (e-capability-id cap) 'goodnite))
    (should (= (length (e-capability-resource-methods cap)) 1))
    (should (string-match-p "using-goodnite" (e-capability-instructions cap)))
    (should (string-match-p "goodnite://" (e-capability-instructions cap)))
    (should (eq (e-layer-id layer) 'goodnite))
    (should (equal (mapcar #'e-capability-id (e-layer-capabilities layer))
                   '(goodnite)))))

(ert-deftest e-goodnite-resources-test-registers-read-glob-search ()
  "Registering the capability's methods yields goodnite read/glob/search."
  (let ((registry (e-resources-registry-create)))
    (e-goodnite-resources-register-resource-methods registry)
    (dolist (op (list e-operation-read e-operation-glob e-operation-search))
      (should (cl-some (lambda (method)
                         (equal (e-resource-method-scheme method) "goodnite"))
                       (e-resources-methods-for-operation registry op))))))

(provide 'e-goodnite-resources-test)

;;; e-goodnite-resources-test.el ends here
