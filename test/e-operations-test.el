;;; e-operations-test.el --- Tests for e resource operation contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for standard resource operation contracts.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-operations)

(ert-deftest e-operations-test-standard-operation-contracts ()
  "Standard operations expose stable model-facing contracts."
  (should (eq (e-operation-id e-operation-read) 'read))
  (should (equal (e-operation-tool-name e-operation-read) "read"))
  (should (string-match-p "URI-addressed resource"
                          (e-operation-description e-operation-read)))
  (should (equal (plist-get (e-operation-parameters e-operation-read) :required)
                 ["uri"]))
  (let* ((range (plist-get
                 (plist-get (e-operation-parameters e-operation-read) :properties)
                 :range))
         (range-properties (plist-get range :properties)))
    (should (equal (plist-get range :type) "object"))
    (should (plist-member range-properties :unit))
    (should (plist-member range-properties :start))
    (should (plist-member range-properties :end))
    (should (plist-member range-properties :limit)))
  (should (eq (e-operation-id e-operation-write) 'write))
  (should (equal (e-operation-tool-name e-operation-write) "write"))
  (should (eq (e-operation-id e-operation-edit) 'edit))
  (should (equal (e-operation-tool-name e-operation-edit) "edit")))

(ert-deftest e-operations-test-dispatchers-normalize-tool-arguments ()
  "Operation dispatchers adapt model tool arguments to resource calls."
  (let (calls)
    (funcall (e-operation-dispatch e-operation-read)
             (lambda (&rest args) (push args calls) "read-result")
             '(:uri "test://read" :range (:unit "line" :start 1 :end 2)))
    (funcall (e-operation-dispatch e-operation-write)
             (lambda (&rest args) (push args calls) "write-result")
             '(:uri "test://write" :content "content"))
    (funcall (e-operation-dispatch e-operation-edit)
             (lambda (&rest args) (push args calls) "edit-result")
             '(:uri "test://edit" :edits ((:oldText "a" :newText "b"))))
    (should (equal (nreverse calls)
                   '(("test://read" (:unit "line" :start 1 :end 2))
                     ("test://write" "content")
                     ("test://edit" ((:oldText "a" :newText "b"))))))))

(provide 'e-operations-test)

;;; e-operations-test.el ends here
