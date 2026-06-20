;;; e-resource-patterns-test.el --- Tests for resource pattern facade -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the portable glob/search facade used by resource discovery.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-base-tools)
(require 'e-emacs-tools)
(require 'e-harness)
(require 'e-resources)
(require 'e-resource-patterns)
(require 'e-session-tmp-resources)
(require 'e-store)

(ert-deftest e-resource-patterns-test-glob-facade-semantics ()
  "Glob patterns use the resource facade language."
  (should (e-resource-pattern-glob-match-p "*.el" "e.el"))
  (should-not (e-resource-pattern-glob-match-p "*.el" "lisp/e.el"))
  (should (e-resource-pattern-glob-match-p "**/*.el" "lisp/core/e.el"))
  (should (e-resource-pattern-glob-match-p "**/plan.org" "plan.org"))
  (should (e-resource-pattern-glob-match-p "**/plan.org"
                                           "docs/feats/25/plan.org"))
  (should-not (e-resource-pattern-glob-match-p "docs/*.org"
                                               "docs/feats/plan.org"))
  (should (equal (e-resource-pattern-glob-fd-pattern "literal[abc].txt")
                 "literal\\[abc\\].txt"))
  (should-error (e-resource-pattern-compile-glob "/absolute/*")
                :type 'e-resource-pattern-invalid)
  (should-error (e-resource-pattern-compile-glob "foo**bar")
                :type 'e-resource-pattern-invalid))

(ert-deftest e-resource-patterns-test-search-facade-semantics ()
  "Search queries escape backend regex syntax and expose facade wildcards."
  (let ((regexp (e-resource-pattern-search-emacs-regexp
                 "e-resource-* method"
                 nil)))
    (should (string-match-p regexp "e-resource-glob method"))
    (should (string-match-p regexp "e-resource- method"))
    (should-not (string-match-p regexp "e-resource-glob other")))

  (let ((regexp (e-resource-pattern-search-emacs-regexp
                 "call(foo)+"
                 '(:case-sensitive t))))
    (should (string-match-p regexp "call(foo)+"))
    (should-not (string-match-p regexp "callfoooo")))

  (let ((case-fold-search t)
        (regexp (e-resource-pattern-search-emacs-regexp
                 "Resource Glob"
                 nil)))
    (should (string-match-p regexp "resource glob")))

  (let ((regexp (e-resource-pattern-search-emacs-regexp
                 "alpha beta"
                 nil)))
    (should (string-match-p regexp "alpha   beta"))
    (should-not (string-match-p regexp "alpha\nbeta")))

  (let ((regexp (e-resource-pattern-search-emacs-regexp
                 "alpha beta"
                 '(:multiline t))))
    (should (string-match-p regexp "alpha\nbeta")))

  (let ((regexp (e-resource-pattern-search-emacs-regexp
                 "needle"
                 '(:whole-word t))))
    (should (string-match-p regexp "a needle here"))
    (should-not (string-match-p regexp "needles"))))

(ert-deftest e-resource-patterns-test-search-equivalence-across-resource-schemes ()
  "The same facade search query works across file, tmp, buffer, and e://."
  (let* ((directory (make-temp-file "e-pattern-file-" t))
         (file-root (expand-file-name "docs" directory))
         (file-registry (e-resources-registry-create))
         (tmp-harness (e-harness-create
                       :backend (e-backend-fake-create :items nil)
                       :active-layers
                       (list (e-layer-create
                              :id 'tmp-layer
                              :name "Tmp Layer"
                              :capabilities
                              (list (e-session-tmp-capability-create))))))
         (tmp-resources
          (e-harness-resources tmp-harness "pattern-session" "pattern-turn"))
         (buffer-registry (e-resources-registry-create))
         (buffer (generate-new-buffer "e-pattern-docs/item.txt"))
         (store (e-store-create))
         (store-registry (e-resources-registry-create))
         (content "Alpha resource method\nBeta\n")
         (options '(:glob "docs/*.txt" :limit 5)))
    (unwind-protect
        (progn
          (make-directory file-root t)
          (write-region content nil
                        (expand-file-name "item.txt" file-root)
                        nil
                        'silent)
          (e-base-tools-register-file-read-resource file-registry directory)
          (e-resources-write tmp-resources "tmp://docs/item.txt" content)
          (with-current-buffer buffer
            (insert content))
          (e-emacs-tools-register-buffer-read-resource buffer-registry)
          (e-store-register store 'pattern "docs/item.txt" :content content)
          (e-resources-register store-registry (e-store-resource-methods store))
          (dolist (case `((,file-registry . "file://")
                          (,tmp-resources . "tmp://")
                          (,buffer-registry . "buffer://e-pattern-")
                          (,store-registry . "e://pattern")))
            (let* ((registry (car case))
                   (uri (cdr case))
                   (matches (plist-get
                             (e-resources-search
                              registry
                              uri
                              "resource method"
                              options)
                             :matches)))
              (should (equal (length matches) 1))
              (should (equal (plist-get (elt matches 0) :line) 1))
              (should (equal (plist-get (elt matches 0) :column) 7))
              (should (equal (plist-get (elt matches 0) :text)
                             "Alpha resource method")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(provide 'e-resource-patterns-test)

;;; e-resource-patterns-test.el ends here
