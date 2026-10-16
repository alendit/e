;;; e-context-canvas-test.el --- Exact canvas revision tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-context-canvas)

(ert-deftest e-context-canvas-test-capture-is-property-free-and-exact ()
  (let* ((propertized (propertize "A🪵\n" 'face 'bold))
         (capture (e-context-canvas-capture-create
                   :uri "file:///tmp/canvas.org"
                   :label "Daily"
                   :id "daily"
                   :content propertized))
         (text (plist-get capture :canvas-text)))
    (should (equal text "A🪵\n"))
    (should-not (text-properties-at 0 text))
    (should (equal (plist-get capture :canvas-revision)
                   (upcase (secure-hash 'sha256 text))))
    (should (equal (plist-get capture :content-sha256)
                   (plist-get capture :canvas-revision)))
    (should (= (plist-get capture :canvas-payload-bytes)
               (string-bytes (encode-coding-string text 'utf-8 t))))
    (should (equal (e-context-source-handle capture)
                   (concat "src:" (plist-get capture :id))))))

(ert-deftest e-context-canvas-test-linear-exact-patch-cases ()
  (dolist (case '(("" "x")
                  ("x" "")
                  ("a🪵b" "a✅b")
                  ("one\ntwo\n" "one\nthree\n")
                  ("prefix" "prefix")
                  ("a" "a repeated unrelated phrase b")))
    (let* ((old (car case))
           (new (cadr case))
           (patch (e-context-canvas-exact-patch old new)))
      (should (equal (e-context-canvas-apply-patch old patch) new))))
  (let* ((old (concat (make-string 100000 ?a) "x" (make-string 100000 ?z)))
         (new (concat (make-string 100000 ?a) "y" (make-string 100000 ?z)))
         (patch (e-context-canvas-exact-patch old new)))
    (should (equal patch (list :start 100000 :end 100001 :replacement "y")))
    (should (equal (e-context-canvas-apply-patch old patch) new)))
  (let* ((patch (e-context-canvas-exact-patch
                 "A old X old B" "A new X new B")))
    (should (equal (e-context-canvas-apply-patch "A old X old B" patch)
                   "A new X new B"))
    (should (equal patch
                   (list :start 2 :end 11 :replacement "new X new")))))

(ert-deftest e-context-canvas-test-payload-byte-count-includes-envelope ()
  (let* ((capture (e-context-canvas-capture-create
                   :uri "file:///tmp/daily.org"
                   :label "A Daily"
                   :id "daily"
                   :content "Hi 🪵"))
         (snapshot (e-context-canvas-snapshot-payload capture))
         (patch (e-context-canvas-exact-patch "Hi 🪵" "Hi ✅"))
         (change (e-context-canvas-patch-payload
                  (e-context-canvas-capture-create
                   :uri "file:///tmp/daily.org"
                   :label "A Daily" :id "daily" :content "Hi ✅")
                  (plist-get capture :canvas-revision) patch)))
    (should (= (e-context-canvas-utf8-bytes snapshot)
               (string-bytes (encode-coding-string snapshot 'utf-8 t))))
    (should (> (e-context-canvas-utf8-bytes change)
               (e-context-canvas-utf8-bytes "✅")))
    (should (string-match-p "baseRevision=" change))))

(provide 'e-context-canvas-test)

;;; e-context-canvas-test.el ends here
