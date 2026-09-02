;;; e-session-board-policy-test.el --- Board-routing policy mechanism tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Direct mechanism tests for the extracted board-routing policy owner.  The
;; first historical test also exercises the aggregate's public admission seam;
;; it uses no aggregate-private or foreign policy implementation symbols.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'e-session-aggregate)
(require 'e-session-codec)
(require 'e-session-board-policy)

(ert-deftest e-session-aggregate-test-board-routing-policy-budget-is-pre-encoding-and-bounded ()
  "Routing admission rejects exact overages before encoding or mutation."
  (let* ((store (e-session-store-create))
         (session-id "routing-budget")
         (policy '(:participant-id "p"
                   :pickup-selector (:tags (private)
                                    :attributes (:marker "123456789"))
                   :observer-selector (:tags (private))
                   :default-tags (private)
                   :default-to nil)))
    (e-session-aggregate-create store :id session-id)
    (e-session-aggregate-declare-board-state
     store session-id "chat:routing-budget" "budget-board" "owner")
    (let ((e-session-board-policy--byte-budget 11)
          encoded
          (original-json-encode (symbol-function 'json-encode)))
      (cl-letf (((symbol-function 'json-encode)
                 (lambda (value)
                   (setq encoded t)
                   (funcall original-json-encode value))))
        (should (e-session-board-policy--value-budget-valid-p
                 "123456789"))
        (let ((e-session-board-policy--byte-budget 10))
          (should-not
           (e-session-board-policy--value-budget-valid-p
            "123456789")))
        (let ((e-session-board-policy--byte-budget 10))
          (should-error
           (e-session-aggregate-declare-board-state
            store session-id "chat:routing-budget" "budget-board" "owner"
            policy)
           :type 'error))
        (should-not encoded)))
    (let ((e-session-board-policy--node-budget 3))
      (should (e-session-board-policy--value-budget-valid-p '(a)))
      (should-not
       (e-session-board-policy--value-budget-valid-p '(a b))))
    (let ((deep nil))
      (dotimes (_ 200)
        (setq deep (list :nested deep)))
      (let ((e-session-board-policy--node-budget 32))
        (should-not
         (e-session-board-policy--value-budget-valid-p deep))))))

(ert-deftest e-session-aggregate-test-board-routing-policy-public-budget-bounds-collections ()
  "Public policy validation bounds hostile tags and vectors before field scans."
  (let* ((node-limit e-session-board-policy--node-budget)
         (huge-tags (make-list (* 4 node-limit) "tag"))
         (huge-vector (make-vector (* 4 node-limit) nil))
         (base
          '(:participant-id "p"
            :pickup-selector (:tags (private))
            :observer-selector (:tags (private))
            :default-tags (private)
            :default-to nil))
         (policies
          (list
           (let ((policy (copy-tree base)))
             (plist-put (plist-get policy :pickup-selector)
                        :tags huge-tags)
             policy)
           (let ((policy (copy-tree base)))
             (plist-put (plist-get policy :pickup-selector)
                        :attributes huge-vector)
             policy))))
    (dolist (policy policies)
      (let ((json-called nil)
            (e-session-board-policy--budget-visit-count 0))
        (cl-letf (((symbol-function 'json-encode)
                   (lambda (&rest _)
                     (setq json-called t)
                     (error "unexpected JSON encoding"))))
          (should-not
           (e-session-board-routing-policy-valid-p policy)))
        (should (<= e-session-board-policy--budget-visit-count
                    (1+ node-limit)))
        (should-not json-called)))))

(ert-deftest e-session-aggregate-test-board-routing-policy-encoded-budget-covers-scalars-and-depth ()
  "The full reversible policy has an exact encoded boundary and safe depth."
  (let* ((policy '(:participant-id "p"
                   :pickup-selector
                   (:kind input :tags (private)
                    :attributes
                    (:symbol car :number 123456789012345678901234567890
                     :nested (car "car" (:inner car))))
                   :observer-selector (:tags (private))
                   :default-tags (private)
                   :default-to nil))
         (encoded (e-session-codec-board-routing-policy-for-json policy))
         (encoded-bytes (e-session-board-policy--json-byte-size
                         encoded)))
    (should (= encoded-bytes
               (string-bytes (json-encode encoded))))
    (let ((e-session-board-policy--byte-budget encoded-bytes))
      (should (e-session-board-routing-policy-valid-p policy)))
    (let ((e-session-board-policy--byte-budget
           (1- encoded-bytes)))
      (should-not (e-session-board-routing-policy-valid-p policy)))
    (should-not
     (e-session-board-policy--value-budget-valid-p
      (intern (make-string
               (1+ e-session-board-policy--byte-budget)
               ?s))))
    (should-not
     (e-session-board-policy--value-budget-valid-p
      (string-to-number
       (concat "1"
               (make-string e-session-board-policy--byte-budget
                            ?0)))))
    (let ((deep 'x))
      (dotimes (_ 300)
        (setq deep (list :nested deep)))
      (should (e-session-board-policy--value-budget-valid-p deep))
      (let* ((deep-policy (e-session-board-routing-policy-copy-value policy))
             (attributes
              (plist-get (plist-get deep-policy :pickup-selector)
                         :attributes))
             (attributes (plist-put attributes :deep deep)))
        (plist-put (plist-get deep-policy :pickup-selector)
                   :attributes attributes)
        (should (e-session-board-routing-policy-valid-p deep-policy))))
    (let ((wide (make-vector
                 (1+ e-session-board-policy--node-budget)
                 nil)))
      (should-not
       (e-session-board-policy--value-budget-valid-p wide)))))

(ert-deftest e-session-board-routing-policy-copy-allows-shared-finite-values ()
  "Copying a finite policy DAG does not mistake sharing for a cycle."
  (let* ((selector '(:tags (main)))
         (policy (list :participant-id "participant"
                       :pickup-selector selector
                       :observer-selector selector
                       :default-tags '(main)
                       :default-to nil))
         (copy (e-session-board-routing-policy-copy-value policy)))
    (should (equal copy policy))
    (should-not (eq (plist-get copy :pickup-selector)
                    (plist-get copy :observer-selector)))))

(provide 'e-session-board-policy-test)

;;; e-session-board-policy-test.el ends here
