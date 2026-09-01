;;; e-session-tmp-sqlite-test.el --- SQLite tmp resource scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-resources)
(require 'e-session-tmp-resources)
(require 'e-tool-invocation-details)

(cl-defmacro e-session-tmp-sqlite-test--with-harness
    ((store harness resources directory) &rest body)
  "Run BODY with an SQLite STORE, HARNESS, and RESOURCES registry."
  (declare (indent 1) (debug ((symbolp symbolp symbolp symbolp) body)))
  `(let* ((,directory (make-temp-file "e-session-tmp-sqlite-" t))
          (,store (e-session-sqlite-store-create ,directory))
          (,harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :sessions ,store
                     :intrinsic-capabilities
                     (list (e-session-tmp-capability-create))))
          (,resources nil))
     (e-harness-create-session ,harness :id "session-1")
     (setq ,resources (e-harness-resources ,harness "session-1" "turn-1"))
     (unwind-protect (progn ,@body)
       (ignore-errors (e-session-sqlite-store-close ,store))
       (delete-directory ,directory t))))

(ert-deftest e-session-tmp-sqlite-s4-roundtrip-edit-glob-search-restart ()
  "The tmp resource interface is unchanged while SQLite owns its values."
  (e-session-tmp-sqlite-test--with-harness
      (store harness resources directory)
    (should (equal (e-resources-write resources "tmp://notes/one.txt"
                                      "Alpha needle\n")
                   "tmp://notes/one.txt"))
    (should (equal (e-resources-read resources "tmp://notes/one.txt" nil)
                   "Alpha needle\n"))
    (e-resources-edit resources "tmp://notes/one.txt"
                      '((:oldText "Alpha" :newText "Beta")))
    (should (equal (e-resources-read resources "tmp://notes/one.txt" nil)
                   "Beta needle\n"))
    (let ((glob (e-resources-glob resources "tmp://notes" "*.txt" 5)))
      (should (equal (mapcar (lambda (item) (plist-get item :uri))
                             (append (plist-get glob :resources) nil))
                     '("tmp://notes/one.txt"))))
    (let ((search (e-resources-search resources "tmp://" "needle"
                                      '(:glob "notes/*.txt" :limit 5))))
      (should (= (length (plist-get search :matches)) 1)))
    (e-session-sqlite-store-close store)
    (setq store (e-session-sqlite-store-create directory)
          harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create)))
          resources (e-harness-resources harness "session-1" "turn-2"))
    (should (equal (e-resources-read resources "tmp://notes/one.txt" nil)
                   "Beta needle\n"))))

(ert-deftest e-session-tmp-sqlite-s4-expiry-cleanup-and-session-delete ()
  "Expiry and owner deletion physically purge private resource values."
  (e-session-tmp-sqlite-test--with-harness
      (store harness resources directory)
    (e-resources-write resources "tmp://expired.txt" "old")
    (let ((runtime (e-session-storage-runtime-store store)))
      (e-runtime-store-call
       runtime 'write
       '(:op resource-put :lineage-id "session-1" :session-id "session-1"
         :path "expires.txt" :content "gone" :expires-at 1.0))
      (e-runtime-store-call runtime 'write '(:op resource-expire :now 2.0)))
    (should-not (e-session-tmp-reference-available-p
                 harness "session-1" "tmp://expires.txt"))
    (e-session-delete store "session-1")
    (should-not (e-session-tmp-reference-available-p
                 harness "session-1" "tmp://expired.txt"))))

(ert-deftest e-session-tmp-sqlite-s4-invocation-details-use-no-backing-file ()
  "Complete invocation details remain readable after worker restart."
  (e-session-tmp-sqlite-test--with-harness
      (store harness resources directory)
    (let* ((call '(:id "call-1" :name "echo"
                   :stated-purpose "Echo the complete value"
                   :arguments (:text "hello")))
           (result '(:tool-call-id "call-1" :name "echo" :status ok
                     :content "complete output" :metadata (:tokens 7)))
           (uri (e-tool-invocation-details-write
                 harness "session-1" "turn-1" call result)))
      (should-not (file-directory-p
                   (expand-file-name "sessions" directory)))
      (e-session-sqlite-store-close store)
      (setq store (e-session-sqlite-store-create directory)
            harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :sessions store
                     :intrinsic-capabilities
                     (list (e-session-tmp-capability-create)))
            resources (e-harness-resources harness "session-1" "turn-2"))
      (let ((document
             (e-tool-invocation-details-decode
              (e-resources-read resources uri nil))))
        (should (equal (plist-get document :tool-call-id) "call-1"))
        (should (equal (plist-get (plist-get document :result) :content)
                       "complete output"))))))

(ert-deftest e-session-tmp-sqlite-s4-private-limit-exact-and-one-over ()
  "The private one-BLOB limit accepts exact and rejects one-over explicitly."
  :tags '(:expensive)
  (e-session-tmp-sqlite-test--with-harness
      (store harness resources directory)
    (let* ((limit (* 16 1024 1024))
           (exact (make-string limit ?x)))
      (should (equal (e-resources-write resources "tmp://limit.bin" exact)
                     "tmp://limit.bin"))
      (should (= (plist-get
                  (e-session-tmp--sqlite-get
                   harness "session-1" "limit.bin")
                  :bytes)
                 limit))
      (should-error
       (e-resources-write resources "tmp://over.bin"
                          (concat exact "x"))
       :type 'e-runtime-store-resource-too-large))))

(provide 'e-session-tmp-sqlite-test)

;;; e-session-tmp-sqlite-test.el ends here
