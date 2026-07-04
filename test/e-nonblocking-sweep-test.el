;;; e-nonblocking-sweep-test.el --- Async transition sync sweep tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression coverage for Feature 40's final non-blocking sweep.  The remaining
;; synchronous primitives are reviewed and either guarded from interactive hot
;; paths or confined to dev/test-support style helpers.  This inventory test
;; fails when a new production synchronous wait appears without review.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst e-nonblocking-sweep-test--blocking-symbols
  '("url-retrieve-synchronously"
    "process-file"
    "accept-process-output"
    "call-process"
    "e-session-load-session")
  "Blocking primitive and sync API names tracked by the async sweep.")

(defconst e-nonblocking-sweep-test--expected-counts
  '(("lisp/adapters/anthropic/e-anthropic.el" "accept-process-output" 1)
    ("lisp/adapters/anthropic/e-anthropic.el" "url-retrieve-synchronously" 1)
    ("lisp/adapters/openai/e-openai.el" "accept-process-output" 2)
    ("lisp/core/e-backend.el" "accept-process-output" 1)
    ("lisp/core/e-harness.el" "accept-process-output" 3)
    ("lisp/core/e-loop.el" "accept-process-output" 1)
    ("lisp/core/e-mcp.el" "accept-process-output" 1)
    ("lisp/core/e-mcp.el" "url-retrieve-synchronously" 2)
    ("lisp/core/e-request.el" "accept-process-output" 2)
    ("lisp/core/e-request.el" "process-file" 2)
    ("lisp/core/e-request.el" "url-retrieve-synchronously" 2)
    ("lisp/core/e-session.el" "e-session-load-session" 2)
    ("lisp/core/e-tools.el" "accept-process-output" 2)
    ("lisp/core/e-work.el" "accept-process-output" 1)
    ("lisp/dev/e-dev-perf.el" "accept-process-output" 2)
    ("lisp/dev/e-dev-perf.el" "call-process" 1)
    ("lisp/dev/e-dev-perf.el" "e-session-load-session" 1)
    ("lisp/layers/base/e-base-tools.el" "accept-process-output" 1)
    ("lisp/layers/base/e-base-tools.el" "process-file" 1)
    ("lisp/layers/emacs/e-emacs-tools.el" "accept-process-output" 1)
    ("lisp/layers/harness/e-session-tmp-resources.el" "process-file" 1)
    ("lisp/layers/web/e-web-tools.el" "accept-process-output" 2)
    ("lisp/layers/web/e-web-tools.el" "url-retrieve-synchronously" 1)
    ("lisp/shells/chat/e-chat.el" "accept-process-output" 1)
    ("lisp/shells/chat/e-chat.el" "process-file" 2))
  "Audited blocking primitive counts for Feature 40.")

(defun e-nonblocking-sweep-test--root ()
  "Return the repository root for this test run."
  (file-name-as-directory
   (or (locate-dominating-file
        (or load-file-name default-directory)
        "Eldev")
       default-directory)))

(defun e-nonblocking-sweep-test--record-count (counts file symbol)
  "Increment COUNTS for FILE and SYMBOL."
  (let* ((key (list file symbol))
         (entry (assoc key counts)))
    (if entry
        (setcdr entry (1+ (cdr entry)))
      (push (cons key 1) counts))
    counts))

(defun e-nonblocking-sweep-test--scan ()
  "Return blocking primitive counts in lisp sources."
  (let* ((root (e-nonblocking-sweep-test--root))
         (lisp-root (expand-file-name "lisp" root))
         (regexp (regexp-opt e-nonblocking-sweep-test--blocking-symbols
                             'symbols))
         counts)
    (dolist (file (directory-files-recursively lisp-root "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (setq counts
                (e-nonblocking-sweep-test--record-count
                 counts
                 (file-relative-name file root)
                 (match-string-no-properties 0))))))
    (sort (mapcar (lambda (entry)
                    (append (car entry) (list (cdr entry))))
                  counts)
          (lambda (a b)
            (string< (format "%s:%s" (car a) (cadr a))
                     (format "%s:%s" (car b) (cadr b)))))))

(ert-deftest e-nonblocking-sweep-test-reviewed-blocking-sites ()
  "Every remaining blocking primitive in lisp/ is part of the reviewed inventory."
  (should (equal (e-nonblocking-sweep-test--scan)
                 e-nonblocking-sweep-test--expected-counts)))

(provide 'e-nonblocking-sweep-test)

;;; e-nonblocking-sweep-test.el ends here
