;;; e-session-aggregate-test.el --- Direct session aggregate contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; These tests exercise aggregate-only semantic invariants.  Durable replay,
;; restart, and physical projection behavior live in the facade composition
;; suite; the dedicated contract file covers fresh owner loading.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-session-aggregate)

(ert-deftest e-session-aggregate-test-context-current-path-rejects-broken-links ()
  "Context ownership paths reject missing heads, parents, and cycles."
  (let* ((store (e-session-store-create))
         (session-id "context-path-integrity")
         (session (e-session-aggregate-create store :id session-id))
         (root-id (plist-get session :root-event-id))
         (message
          (e-session-aggregate-append-message
           store session-id
           '(:id "context-path-message" :role assistant :content "path")))
         (message-id (plist-get message :id))
         (root (e-session-aggregate-entry-by-id store session-id root-id)))
    (should-error
     (e-session-aggregate--context-current-path store session-id "missing-head")
     :type 'e-session-error)
    (plist-put message :parent-id "missing-parent")
    (should-error
     (e-session-aggregate--context-current-path store session-id message-id)
     :type 'e-session-error)
    (plist-put message :parent-id root-id)
    (plist-put root :parent-id message-id)
    (should-error
     (e-session-aggregate--context-current-path store session-id message-id)
     :type 'e-session-error)))
(ert-deftest e-session-aggregate-test-message-appends-maintain-derived-fields-incrementally ()
  "Message appends update metadata without full derived-field refreshes."
  (let ((store (e-session-store-create))
        (refresh-count 0)
        (original-refresh (symbol-function 'e-session-aggregate--refresh-derived-fields)))
    (cl-letf (((symbol-function 'e-session-aggregate--refresh-derived-fields)
               (lambda (refresh-store refresh-session)
                 (setq refresh-count (1+ refresh-count))
                 (funcall original-refresh refresh-store refresh-session))))
      (e-session-aggregate-create store :id "session-1")
      (setq refresh-count 0)
      (e-session-aggregate-append-message
       store "session-1"
       '(:id "msg-1" :role user :content "first"))
      (e-session-aggregate-append-message
       store "session-1"
       '(:id "msg-2" :role assistant :content "second"))
      (let ((session (e-session-aggregate-get store "session-1")))
        (should (= refresh-count 0))
        (should (= (plist-get session :message-count) 2))
        (should (equal (plist-get session :summary) "first"))
        (should (equal (plist-get session :last-message-at)
                       (plist-get (cadr (plist-get session :messages))
                                  :created-at)))))))


(ert-deftest e-session-aggregate-test-append-message-stamps-created-at ()
  "Appended messages carry their creation timestamp."
  (let ((store (e-session-store-create)))
    (cl-letf (((symbol-function 'e-session-aggregate--timestamp)
               (lambda (&optional _time) "2026-05-21T10:00:00Z")))
      (e-session-aggregate-create store :id "session-1")
      (e-session-aggregate-append-message
       store "session-1" '(:role user :content "hello"))
      (should (equal (plist-get (car (e-session-aggregate-messages store "session-1"))
                                :created-at)
                     "2026-05-21T10:00:00Z")))))


(ert-deftest e-session-aggregate-test-current-path-uses-entry-index ()
  "Current-path traversal avoids repeated linear entry searches."
  (let ((store (e-session-store-create)))
    (e-session-aggregate-create store :id "session-1")
    (dotimes (index 25)
      (e-session-aggregate-append-message
       store
       "session-1"
       (list :role 'user :content (format "message-%d" index))))
    (let ((calls 0)
          (index (e-session-aggregate--entry-index store "session-1")))
      (cl-letf (((symbol-function 'e-session-aggregate--entries)
                 (lambda (&rest _args)
                   (setq calls (1+ calls))
                   nil)))
        (should (= (length (e-session-aggregate-current-path store "session-1")) 26))
        (should (= calls 0))
        (clrhash index)
        (should-not (e-session-aggregate-current-path store "session-1"))
        (should (> calls 0))))))


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
    (let ((e-session-aggregate--board-routing-policy-byte-budget 11)
          encoded
          (original-json-encode (symbol-function 'json-encode)))
      (cl-letf (((symbol-function 'json-encode)
                 (lambda (value)
                   (setq encoded t)
                   (funcall original-json-encode value))))
        (should (e-session-aggregate--board-routing-value-budget-valid-p
                 "123456789"))
        (let ((e-session-aggregate--board-routing-policy-byte-budget 10))
          (should-not
           (e-session-aggregate--board-routing-value-budget-valid-p
            "123456789")))
        (let ((e-session-aggregate--board-routing-policy-byte-budget 10))
          (should-error
           (e-session-aggregate-declare-board-state
            store session-id "chat:routing-budget" "budget-board" "owner"
            policy)
           :type 'error))
        (should-not encoded)))
    (let ((e-session-aggregate--board-routing-policy-node-budget 3))
      (should (e-session-aggregate--board-routing-value-budget-valid-p '(a)))
      (should-not
       (e-session-aggregate--board-routing-value-budget-valid-p '(a b))))
    (let ((deep nil))
      (dotimes (_ 200)
        (setq deep (list :nested deep)))
      (let ((e-session-aggregate--board-routing-policy-node-budget 32))
        (should-not
         (e-session-aggregate--board-routing-value-budget-valid-p deep))))))

(ert-deftest e-session-aggregate-test-board-routing-policy-public-budget-bounds-collections ()
  "Public policy validation bounds hostile tags and vectors before field scans."
  (let* ((node-limit e-session-aggregate--board-routing-policy-node-budget)
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
            (e-session-aggregate--board-routing-budget-visit-count 0))
        (cl-letf (((symbol-function 'json-encode)
                   (lambda (&rest _)
                     (setq json-called t)
                     (error "unexpected JSON encoding"))))
          (should-not
           (e-session-aggregate-board-routing-policy-valid-p policy)))
        (should (<= e-session-aggregate--board-routing-budget-visit-count
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
         (encoded-bytes (e-session-aggregate--board-routing-json-byte-size
                         encoded)))
    (should (= encoded-bytes
               (string-bytes (json-encode encoded))))
    (let ((e-session-aggregate--board-routing-policy-byte-budget encoded-bytes))
      (should (e-session-aggregate-board-routing-policy-valid-p policy)))
    (let ((e-session-aggregate--board-routing-policy-byte-budget
           (1- encoded-bytes)))
      (should-not (e-session-aggregate-board-routing-policy-valid-p policy)))
    (should-not
     (e-session-aggregate--board-routing-value-budget-valid-p
      (intern (make-string
               (1+ e-session-aggregate--board-routing-policy-byte-budget)
               ?s))))
    (should-not
     (e-session-aggregate--board-routing-value-budget-valid-p
      (string-to-number
       (concat "1"
               (make-string e-session-aggregate--board-routing-policy-byte-budget
                            ?0)))))
    (let ((deep 'x))
      (dotimes (_ 300)
        (setq deep (list :nested deep)))
      (should (e-session-aggregate--board-routing-value-budget-valid-p deep))
      (let* ((deep-policy (e-session-aggregate-board-routing-copy-value policy))
             (attributes
              (plist-get (plist-get deep-policy :pickup-selector)
                         :attributes))
             (attributes (plist-put attributes :deep deep)))
        (plist-put (plist-get deep-policy :pickup-selector)
                   :attributes attributes)
        (should (e-session-aggregate-board-routing-policy-valid-p deep-policy))))
    (let ((wide (make-vector
                 (1+ e-session-aggregate--board-routing-policy-node-budget)
                 nil)))
      (should-not
       (e-session-aggregate--board-routing-value-budget-valid-p wide)))))

(provide 'e-session-aggregate-test)

;;; e-session-aggregate-test.el ends here
