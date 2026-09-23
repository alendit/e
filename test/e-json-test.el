;;; e-json-test.el --- Tests for canonical JSON values -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the canonical JSON representation and its wire boundary.

;;; Code:

(require 'ert)
(require 'e-json)

(ert-deftest e-json-test-sentinels-and-value-shapes ()
  "Canonical values use exact sentinels and unambiguous containers."
  (should (eq e-json-false :json-false))
  (should (eq e-json-null :json-null))
  (dolist (value (list nil [] t e-json-false e-json-null "text" 42 -3.5))
    (should (e-json-value-p value)))
  (should (e-json-value-p
           '(:name "daily"
             :enabled t
             :missing :json-null
             :sections [(:title "one" :gaps [])])))
  (should (e-json-value-p []))
  (should (e-json-value-p nil)))

(ert-deftest e-json-test-rejects-noncanonical-containers-and-symbols ()
  "Lists, alists, hashes, and arbitrary symbols never become JSON values."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (value (list '(1 2)
                         '("key" 1)
                         '(a . 1)
                         '((a . 1))
                         hash
                         'arbitrary-symbol
                         :other
                         '(:key)
                         '(:key 1 . :tail)))
      (should-not (e-json-value-p value))
      (should-error (e-json-assert-value value)
                    :type 'e-json-error))))

(ert-deftest e-json-test-rejects-duplicate-object-keys ()
  "Canonical objects cannot contain duplicate keyword keys."
  (should-not (e-json-value-p '(:name "one" :name "two")))
  (should-error (e-json-assert-value '(:name "one" :name "two"))
                :type 'e-json-error))

(ert-deftest e-json-test-rejects-nonfinite-numbers ()
  "NaN and infinities are not canonical JSON numbers."
  (let ((nan (sqrt -1.0))
        (infinity 1.0e+INF))
    (dolist (value (list nan infinity (- infinity)))
      (should-not (e-json-value-p value))
      (should-error (e-json-assert-value value)
                    :type 'e-json-error))))

(ert-deftest e-json-test-rejects-cycles ()
  "Cyclic lists and vectors are rejected by the canonical assertion."
  (let ((plist (list :self nil))
        (array (vector nil)))
    (setcar (cdr plist) plist)
    (aset array 0 array)
    (dolist (value (list plist array))
      (should-not (e-json-value-p value))
      (should-error (e-json-assert-value value)
                    :type 'e-json-error))))

(ert-deftest e-json-test-parses-canonical-shapes-once ()
  "Parsing produces plist objects, vector arrays, and exact sentinels."
  (let* ((value
          (e-json-parse-string
           "{\"empty-object\":{},\"empty-array\":[],\"false\":false,\"null\":null,\"items\":[{\"id\":1,\"nested\":{}}]}"))
         (items (plist-get value :items)))
    (should (e-json-value-p value))
    (should (null (plist-get value :empty-object)))
    (should (vectorp (plist-get value :empty-array)))
    (should (eq (plist-get value :false) e-json-false))
    (should (eq (plist-get value :null) e-json-null))
    (should (vectorp items))
    (should (= (length items) 1))
    (should (equal (aref items 0) '(:id 1 :nested nil)))))

(ert-deftest e-json-test-parse-rejects-duplicate-keys-and-invalid-json ()
  "Parsing rejects duplicate object keys and malformed wire values."
  (dolist (text (list "{\"name\":1,\"name\":2}"
                      "{\"name\":1,\"n\\u0061me\":2}"
                      "{\"name\":}"))
    (should-error (e-json-parse-string text)
                  :type 'e-json-error))
  (should-error (e-json-parse-string nil)
                :type 'e-json-error))

(ert-deftest e-json-test-serializes-distinct-empty-and-sentinel-values ()
  "Serialization preserves empty object, empty array, false, and null."
  (should (equal (e-json-serialize nil) "{}"))
  (should (equal (e-json-serialize []) "[]"))
  (should (equal (e-json-serialize e-json-false) "false"))
  (should (equal (e-json-serialize e-json-null) "null")))

(ert-deftest e-json-test-round-trips-nested-values-and-arrays-of-objects ()
  "Canonical nested values round-trip without container reshaping."
  (let* ((value '(:title "Daily"
                  :sections [(:title "one" :gaps [:json-null])
                             (:title "two" :gaps [])]
                  :empty-object nil
                  :empty-array []
                  :false :json-false
                  :null :json-null))
         (wire (e-json-serialize value))
         (round-trip (e-json-parse-string wire)))
    (should (equal round-trip value))
    (should (vectorp (plist-get round-trip :sections)))
    (should (null (plist-get round-trip :empty-object)))
    (should (equal (plist-get round-trip :empty-array) []))))

(ert-deftest e-json-test-unicode-serialization-produces-composable-text ()
  "Serialized Unicode JSON remains valid as nested canonical text."
  (let* ((value '(:summary "Daily — complete"))
         (text (e-json-serialize value))
         (outer-text (e-json-serialize (list :content text)))
         (outer (e-json-parse-string outer-text)))
    (should (multibyte-string-p text))
    (should (equal (e-json-parse-string text) value))
    (should (equal (plist-get outer :content) text))
    (should (equal (e-json-parse-string (plist-get outer :content))
                   value))))

(ert-deftest e-json-test-rejects-non-ascii-raw-byte-strings ()
  "Canonical JSON rejects byte strings whose character encoding is unknown."
  (let* ((bytes (encode-coding-string "Daily — complete" 'utf-8 t))
         (eight-bit-text
          (string-as-multibyte (unibyte-string #xc0 #xc1))))
    (should-not (multibyte-string-p bytes))
    (should-not (e-json-value-p bytes))
    (should (multibyte-string-p eight-bit-text))
    (should-not (e-json-value-p eight-bit-text))
    (should-error (e-json-assert-value bytes)
                  :type 'e-json-error)
    (should-error (e-json-assert-value eight-bit-text)
                  :type 'e-json-error)
    (should-error (e-json-serialize (list :content bytes))
                  :type 'e-json-error)))

(ert-deftest e-json-test-serialization-rejects-noncanonical-values ()
  "Serialization asserts the same exact boundary as direct validation."
  (dolist (value (list '(1 2) '((a . 1)) :other))
    (should-error (e-json-serialize value)
                  :type 'e-json-error)))

(ert-deftest e-json-test-schema-validates-canonical-values-without-reshaping ()
  "The shared schema subset validates nested canonical values unchanged."
  (let* ((schema '(:type "object"
                   :properties (:name (:type "string" :minLength 1)
                                :enabled (:type "boolean")
                                :sections (:type "array"
                                            :items (:type "object"
                                                    :required ["title"]
                                                    :properties
                                                    (:title (:type "string")
                                                     :gaps (:type "array"
                                                            :items (:type "null"))))))
                   :required ["name" "sections"]
                   :additionalProperties :json-false))
         (value '(:name "Daily"
                  :enabled :json-false
                  :sections [(:title "one" :gaps [:json-null])])))
    (should (eq (e-json-schema-assert value schema) value))
    (should (e-json-schema-value-p value schema))
    (should (e-json-schema-value-p '(:name "Daily" :sections [] ) schema))
    (dolist (invalid (list '(:sections [])
                           '(:name "" :sections [])
                           '(:name "Daily" :sections [(:gaps [])])
                           '(:name "Daily" :sections [] :extra t)))
      (should-error (e-json-schema-assert invalid schema)
                    :type 'e-json-schema-error))
    (should-error (e-json-schema-assert
                   '(:name "Daily" :sections ((:title "one")))
                   schema)
                  :type 'e-json-error)))

(ert-deftest e-json-test-schema-rejects-noncanonical-schema-shapes ()
  "Schema objects and arrays use the same strict canonical representation."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (schema (list '(:type "object" :properties ((:name . (:type "string"))))
                         '(:type "object" :properties #s(hash-table test equal data ()))
                         '(:type "array" :items ((:type "string")))
                         '(:type "object" :required ("name"))
                         '(:type "object" :unknown t)
                         (list :type "object" :properties hash)))
      (should-error (e-json-schema-assert-schema schema)
                    :type 'e-json-error))))

(provide 'e-json-test)

;;; e-json-test.el ends here
