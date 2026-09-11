;;; e-session-board-policy-test.el --- Board-routing policy mechanism tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Direct mechanism tests for the extracted board-routing policy owner.  These
;; tests exercise the pure policy boundary without constructing a session
;; or Board aggregate.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-runtime-store-codec)
(require 'e-session-board-policy)

(ert-deftest e-session-board-policy-test-budget-is-pre-encoding-and-bounded ()
  "Routing admission rejects exact overages before encoding or mutation."
  (let* ((policy '(:participant-id "p"
                   :pickup-selector (:tags (private)
                                    :attributes (:marker "123456789"))
                   :observer-selector (:tags (private))
                   :default-tags (private)
                   :default-to nil)))
    (let ((e-session-board-policy--byte-budget 11)
          encoded
          (original-encode
           (symbol-function 'e-runtime-store-codec-encode)))
      (cl-letf (((symbol-function 'e-runtime-store-codec-encode)
                 (lambda (value)
                   (setq encoded t)
                   (funcall original-encode value))))
        (should (e-session-board-policy--value-budget-valid-p
                 "123456789"))
        (let ((e-session-board-policy--byte-budget 10))
          (should-not
           (e-session-board-policy--value-budget-valid-p
            "123456789")))
        (let ((e-session-board-policy--byte-budget 10))
          (should-not (e-session-board-routing-policy-valid-p policy)))
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

(ert-deftest e-session-board-policy-test-public-budget-bounds-collections ()
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
        (cl-letf (((symbol-function 'e-runtime-store-codec-encode)
                   (lambda (&rest _)
                     (setq json-called t)
                     (error "unexpected durable encoding"))))
          (should-not
           (e-session-board-routing-policy-valid-p policy)))
        (should (<= e-session-board-policy--budget-visit-count
                    (1+ node-limit)))
        (should-not json-called)))))

(ert-deftest e-session-board-policy-test-encoded-budget-covers-scalars-and-depth ()
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
         (encoded-bytes
          (e-runtime-store-codec-measure-bounded
           policy most-positive-fixnum)))
    (should (= encoded-bytes
               (string-bytes (e-runtime-store-codec-encode policy))))
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
      (dotimes (_ 24)
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
    (let ((pathological-depth 'x))
      (dotimes (_ 5000)
        (setq pathological-depth (list :nested pathological-depth)))
      ;; The current durable codec accepts deep finite values, so admission is
      ;; governed by the explicit structural-node budget rather than by an
      ;; accidental recursion limit from the retired JSON wire measurer.
      (let* ((deep-policy (e-session-board-routing-policy-copy-value policy))
             (attributes
              (plist-get (plist-get deep-policy :pickup-selector)
                         :attributes)))
        (plist-put attributes :deep pathological-depth)
        (should-not (e-session-board-routing-policy-valid-p deep-policy))))
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
