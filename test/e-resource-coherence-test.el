;;; e-resource-coherence-test.el --- Tests for generic coherence model -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for generic linked-resource coherence contracts.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-resource-coherence)

(ert-deftest e-resource-coherence-test-provider-dispatches-by-scheme ()
  "Coherence registries dispatch to providers by URI scheme."
  (let ((registry (e-resource-coherence-registry-create)))
    (e-resource-coherence-register
     registry
     (e-resource-coherence-provider-create
      :id 'demo
      :schemes '("demo")
      :handler (lambda (uri)
                 (e-resource-coherence-group-create
                  :canonical-uri "demo://canonical"
                  :subject-uri (plist-get uri :uri)
                  :views nil))))
    (should (equal (plist-get (e-resource-coherence-group
                               registry "demo://view")
                              :canonical-uri)
                   "demo://canonical"))
    (should-error (e-resource-coherence-group registry "missing://view")
                  :type 'e-resource-coherence-unsupported)))

(ert-deftest e-resource-coherence-test-group-status-aggregates-view-state ()
  "Group status summarizes linked view state."
  (should (eq (e-resource-coherence-group-status
               (e-resource-coherence-group-create
                :views (list (e-resource-coherence-view-create
                              :uri "demo://a"
                              :status 'coherent)
                             (e-resource-coherence-view-create
                              :uri "demo://b"
                              :status 'needs-save))))
              'needs-save))
  (should (eq (e-resource-coherence-group-status
               (e-resource-coherence-group-create
                :views (list (e-resource-coherence-view-create
                              :uri "demo://a"
                              :status 'coherent)
                             (e-resource-coherence-view-create
                              :uri "demo://b"
                              :status 'stale))))
              'stale)))

(ert-deftest e-resource-coherence-test-preferred-view-uses-live-visibility ()
  "Preferred view selection is generic over view metadata."
  (let ((visible (e-resource-coherence-view-create
                  :uri "demo://visible"
                  :label "visible"
                  :live t
                  :visible t))
        (hidden (e-resource-coherence-view-create
                 :uri "demo://hidden"
                 :label "hidden"
                 :live t)))
    (should (equal (plist-get (e-resource-coherence-preferred-view
                               (list hidden visible))
                              :uri)
                   "demo://visible")))
  (should-error
   (e-resource-coherence-preferred-view
    (list (e-resource-coherence-view-create
           :uri "demo://a"
           :label "a"
           :live t)
          (e-resource-coherence-view-create
           :uri "demo://b"
           :label "b"
           :live t)))
   :type 'e-resource-coherence-conflict))

(ert-deftest e-resource-coherence-test-dirty-linked-views-conflict ()
  "Generic dirty-view policy rejects unsafe writes."
  (let ((group (e-resource-coherence-group-create
                :canonical-uri "demo://entity"
                :subject-uri "demo://persisted"
                :views (list (e-resource-coherence-view-create
                              :uri "demo://persisted"
                              :label "persisted"
                              :status 'coherent)
                             (e-resource-coherence-view-create
                              :uri "demo://live"
                              :label "live"
                              :status 'needs-save
                              :modified t)))))
    (should-error
     (e-resource-coherence-conflict-if-dirty group "demo://persisted")
     :type 'e-resource-coherence-conflict)
    (should-not
     (e-resource-coherence-conflict-if-dirty group "demo://live"))))

(provide 'e-resource-coherence-test)

;;; e-resource-coherence-test.el ends here
