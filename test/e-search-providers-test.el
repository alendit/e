;;; e-search-providers-test.el --- Tests for pluggable search providers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the search provider registry seam.

;;; Code:

(require 'ert)
(require 'e-search-providers)

(defun e-search-providers-test--provider (id priority claim)
  "Return a provider named ID with PRIORITY that claims when CLAIM matches."
  (e-search-provider-create
   :id id
   :priority priority
   :predicate (lambda (request)
                (equal (plist-get request :absolute-path) claim))
   :search (lambda (_request) (list :matches (vector id) :truncated nil))))

(ert-deftest e-search-providers-test-registers-and-claims ()
  "A registered provider claims a matching request and runs its search."
  (e-search-providers-reset)
  (unwind-protect
      (progn
        (e-search-providers-register
         (e-search-providers-test--provider 'alpha 0 "/tmp/a"))
        (let ((provider (e-search-providers-provider-for
                         '(:absolute-path "/tmp/a"))))
          (should provider)
          (should (eq (e-search-provider-id provider) 'alpha))
          (should (equal (e-search-providers-run
                          provider '(:absolute-path "/tmp/a"))
                         '(:matches [alpha] :truncated nil))))
        (should-not (e-search-providers-provider-for
                     '(:absolute-path "/tmp/other"))))
    (e-search-providers-reset)))

(ert-deftest e-search-providers-test-priority-orders-claims ()
  "The highest-priority claiming provider wins."
  (e-search-providers-reset)
  (unwind-protect
      (progn
        (e-search-providers-register
         (e-search-providers-test--provider 'low 1 "/tmp/a"))
        (e-search-providers-register
         (e-search-providers-test--provider 'high 10 "/tmp/a"))
        (should (eq (e-search-provider-id
                     (e-search-providers-provider-for '(:absolute-path "/tmp/a")))
                    'high)))
    (e-search-providers-reset)))

(ert-deftest e-search-providers-test-register-replaces-same-id ()
  "Re-registering an id replaces the previous provider rather than duplicating."
  (e-search-providers-reset)
  (unwind-protect
      (progn
        (e-search-providers-register
         (e-search-providers-test--provider 'alpha 0 "/tmp/a"))
        (e-search-providers-register
         (e-search-providers-test--provider 'alpha 0 "/tmp/b"))
        (should (= (length (e-search-providers-list)) 1))
        (should-not (e-search-providers-provider-for '(:absolute-path "/tmp/a")))
        (should (e-search-providers-provider-for '(:absolute-path "/tmp/b"))))
    (e-search-providers-reset)))

(ert-deftest e-search-providers-test-unregister ()
  "Unregistering removes a provider."
  (e-search-providers-reset)
  (unwind-protect
      (progn
        (e-search-providers-register
         (e-search-providers-test--provider 'alpha 0 "/tmp/a"))
        (e-search-providers-unregister 'alpha)
        (should (null (e-search-providers-list))))
    (e-search-providers-reset)))

(ert-deftest e-search-providers-test-under-root-predicate ()
  "The under-root predicate claims paths at or below a root."
  (let ((claim (e-search-providers-under-root-predicate "/tmp/repo")))
    (should (funcall claim '(:absolute-path "/tmp/repo")))
    (should (funcall claim '(:absolute-path "/tmp/repo/topics/x.org")))
    (should-not (funcall claim '(:absolute-path "/tmp/other")))
    (should-not (funcall claim '(:absolute-path "/tmp/repository")))))

(ert-deftest e-search-providers-test-predicate-error-is-skipped ()
  "A predicate that errors does not claim the request."
  (e-search-providers-reset)
  (unwind-protect
      (progn
        (e-search-providers-register
         (e-search-provider-create
          :id 'boom
          :priority 100
          :predicate (lambda (_request) (error "boom"))
          :search (lambda (_request) '(:matches [] :truncated nil))))
        (should-not (e-search-providers-provider-for '(:absolute-path "/tmp/a"))))
    (e-search-providers-reset)))

(provide 'e-search-providers-test)

;;; e-search-providers-test.el ends here
