;;; e-session-persistence-test.el --- Tests for async session persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-session)
(require 'e-session-persistence)

(ert-deftest e-session-persistence-test-writer-commits-journal-and-catalog ()
  "The writer owns durable JSONL and catalog work outside the Emacs mutation path."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-persistence-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store))
         (writes 0)
         (original-write-region (symbol-function 'write-region)))
    (unwind-protect
        (cl-letf (((symbol-function 'write-region)
                   (lambda (&rest arguments)
                     (setq writes (1+ writes))
                     (apply original-write-region arguments))))
          (e-session-create store :id "session-1")
          (e-session-append-message
           store "session-1" '(:role user :content "writer owned"))
          ;; No session file operation is performed by this Emacs process.
          (should (= writes 0))
          (e-session-flush store 5)
          (should (= writes 0))
          (should (string-match-p "^[0-9A-Z]+:[0-9]+\\'"
                                  (e-session-persistence-submit-record
                                   controller "session-1"
                                   '(:type "session-info" :metadata (:retry t)))))
          (e-session-flush store 5)
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (plist-get (car (e-session-messages loaded "session-1"))
                                      :content)
                           "writer owned"))
            (should (file-exists-p (expand-file-name "index.json" directory)))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-stale-catalog-does-not-hide-new-journal ()
  "Startup supplements a catalog snapshot with journal roots it does not contain."
  (let* ((directory (make-temp-file "e-session-reconcile-" t))
         (first (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create first :id "indexed")
          ;; The direct store wrote an index containing only INDEXED.
          (let ((journal (expand-file-name "sessions/unindexed.jsonl" directory)))
            (make-directory (file-name-directory journal) t)
            (with-temp-file journal
              (insert "{\"type\":\"session\",\"session-id\":\"unindexed\",\"timestamp\":\"2026-07-30T00:00:00Z\"}\n")))
          (let ((indexed (e-session-persistent-index-store-create directory)))
            (should (e-session-get indexed "indexed"))
            (should (e-session-get indexed "unindexed"))))
      (delete-directory directory t))))

(ert-deftest e-session-persistence-test-retries-keep-journal-order-and-deduplicate ()
  "Replayed writer commands append once and preserve session record order."
  (skip-unless (executable-find e-session-persistence-node-executable))
  (let* ((directory (make-temp-file "e-session-persistence-retry-" t))
         (store (e-session-persistent-index-store-create directory))
         (controller (e-session-persistence-enable store))
         (sent nil))
    (unwind-protect
        (progn
          ;; Hold commands in the controller outbox, then deliver every one
          ;; twice as a process restart would.  The writer must retain only
          ;; the first delivery of each globally unique command id.
          (cl-letf (((symbol-function 'e-session-persistence--send)
                     (lambda (_controller request) (push request sent))))
            (e-session-create store :id "session-1")
            (e-session-append-message
             store "session-1" '(:role user :content "ordered retry")))
          (dolist (request (nreverse sent))
            (e-session-persistence--send controller request)
            (e-session-persistence--send controller request))
          (e-session-flush store 5)
          (with-temp-buffer
            (insert-file-contents
             (expand-file-name "sessions/session-1.jsonl" directory))
            (should (= (count-lines (point-min) (point-max)) 2)))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (mapcar (lambda (message) (plist-get message :content))
                                   (e-session-messages loaded "session-1"))
                           '("ordered retry")))))
      (when-let ((process (e-session-persistence-process controller)))
        (when (process-live-p process) (kill-process process)))
      (delete-directory directory t))))

(provide 'e-session-persistence-test)

;;; e-session-persistence-test.el ends here
