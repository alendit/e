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
  (should (equal (e-operation-tool-name e-operation-edit) "edit"))
  (should (eq (e-operation-id e-operation-glob) 'glob))
  (should (equal (e-operation-tool-name e-operation-glob) "glob"))
  (should (equal (plist-get (e-operation-parameters e-operation-glob) :required)
                 ["uri"]))
  (should (eq (e-operation-id e-operation-search) 'search))
  (should (equal (e-operation-tool-name e-operation-search) "search"))
  (should (equal (plist-get (e-operation-parameters e-operation-search) :required)
                 ["uri" "query"])))

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
    (funcall (e-operation-dispatch e-operation-glob)
             (lambda (&rest args) (push args calls) "glob-result")
             '(:uri "test://glob"
               :pattern "*.el"
               :limit 5
               :case-sensitive nil))
    (funcall (e-operation-dispatch e-operation-search)
             (lambda (&rest args) (push args calls) "search-result")
             '(:uri "test://search"
               :query "needle"
               :glob "*.el"
               :case-sensitive t
               :whole-word t
               :multiline t
               :limit 7))
    (should (equal (nreverse calls)
                   '(("test://read" (:unit "line" :start 1 :end 2))
                     ("test://write" "content")
                     ("test://edit" ((:oldText "a" :newText "b")))
                     ("test://glob" "*.el" 5 nil)
                     ("test://search" "needle"
                      (:glob "*.el"
                       :case-sensitive t
                       :whole-word t
                       :multiline t
                       :limit 7)))))))

(ert-deftest e-operations-test-edit-coerces-stringified-and-bare-edits ()
  "The edit dispatcher accepts the malformed `edits' shapes providers emit.

Bedrock-routed models JSON-stringify nested tool arguments and sometimes send
a lone edit object instead of a one-element array; both must normalize to a
list of edit plists rather than tripping the resource validator."
  ;; A well-formed array passes through unchanged.
  (should (equal (e-operations--coerce-edits '((:oldText "a" :newText "b")))
                 '((:oldText "a" :newText "b"))))
  ;; A bare edit object is wrapped into a one-element array.
  (should (equal (e-operations--coerce-edits '(:oldText "a" :newText "b"))
                 '((:oldText "a" :newText "b"))))
  ;; A JSON-stringified single object decodes and wraps.
  (should (equal (e-operations--coerce-edits
                  "{\"oldText\": \"a\", \"newText\": \"b\"}")
                 '((:oldText "a" :newText "b"))))
  ;; A JSON-stringified array decodes to the list.
  (should (equal (e-operations--coerce-edits
                  "[{\"oldText\": \"a\", \"newText\": \"b\"}]")
                 '((:oldText "a" :newText "b"))))
  ;; The dispatcher applies the coercion before calling the resource handler.
  (let (calls)
    (funcall (e-operation-dispatch e-operation-edit)
             (lambda (&rest args) (push args calls) "edit-result")
             '(:uri "test://edit"
               :edits "{\"oldText\": \"a\", \"newText\": \"b\"}"))
    (should (equal (nreverse calls)
                   '(("test://edit" ((:oldText "a" :newText "b"))))))))

(provide 'e-operations-test)

;;; e-operations-test.el ends here
