;;; e-session-policy-test.el --- Direct session policy contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; These tests load the pure metadata, identity, provider-anchor, and
;; board-routing owners directly.  They intentionally do not load the session
;; facade or aggregate so a different aggregate implementation can satisfy
;; the same value contracts.

;;; Code:

(require 'ert)
(require 'e-session-metadata)
(require 'e-session-identity)
(require 'e-session-provider-anchor)
(require 'e-session-board-policy)

(ert-deftest e-session-policy-test-pure-owners-load-without-facade-or-aggregate ()
  "Pure session policy owners do not pull in the composition roots."
  (when (or (featurep 'e-session-aggregate)
            (featurep 'e-session))
    (ert-skip "fresh-load contract is exercised in an isolated process"))
  (should (featurep 'e-session-metadata))
  (should (featurep 'e-session-identity))
  (should (featurep 'e-session-provider-anchor))
  (should (featurep 'e-session-board-policy))
  (should-not (featurep 'e-session-aggregate))
  (should-not (featurep 'e-session)))

(ert-deftest e-session-policy-test-metadata-schema-is-closed-and-replay-safe ()
  "Metadata validation and replay normalization keep the durable schema closed."
  (should (e-session-metadata-keyword-plist-p '(:name "session")))
  (should (eq (e-session-metadata-policy-key-state-class :context-references)
             'current-state-reference))
  (should-error (e-session-metadata-validate '(:unknown "value")))
  (should-error
   (e-session-metadata-validate '(:org-canvas (:last-focus 42))))
  (should
   (equal
    (e-session-metadata-normalize-for-replay
     '(:name "session"
       :e-chat-read-markers (:assistant 4)
       :org-canvas (:node-id "node")))
    '(:name "session" :org-canvas (:node-id "node"))))
  (should
   (equal
    (e-session-metadata-context-references-value
     '(:context-references (:canvas [(:id "one")]))
     'canvas)
    '((:id "one")))))

(ert-deftest e-session-policy-test-identity-is-monotonic-and-detached ()
  "Identity owner supplies opaque ordered ids without aggregate state."
  (let (ids)
    (cl-letf (((symbol-function 'float-time)
               (let ((times '(1770000000.001 1770000000.001 1770000000.002)))
                 (lambda (&optional _time)
                   (prog1 (car times)
                     (setq times (or (cdr times) times)))))))
      (setq ids (list (e-session-identity-generate-ulid)
                      (e-session-identity-generate-ulid)
                      (e-session-identity-generate-ulid))))
    (should (seq-every-p
             (lambda (id)
               (string-match-p "\\`[0-9A-HJKMNP-TV-Z]\\{26\\}\\'" id))
             ids))
    (should (equal ids (sort (copy-sequence ids) #'string<)))))

(ert-deftest e-session-policy-test-provider-anchor-is-path-shaped ()
  "Provider-anchor compatibility compares explicit path/value projections."
  (let* ((path '((:id "entry-1") (:id "entry-2")))
         (fingerprints
          '(:segments ((:kind static-prefix :fingerprint "stable")
                       (:kind current-state :fingerprint "volatile"))
            :active-layer-ids (base)
            :tools (read)
            :reasoning (:effort "high")
            :provider-options (:model "gpt-test")
            :compaction-boundary nil
            :lifetime-generation "generation-1"))
         (anchor (list :type 'provider-anchor
                       :provider-id 'openai
                       :model "gpt-test"
                       :id "entry-2"
                       :covered-entry-id "entry-1"
                       :fingerprints (copy-tree fingerprints))))
    (should
     (e-session-provider-anchor-policy-compatible-p
      path anchor 'openai "gpt-test" fingerprints))
    (should
     (eq (e-session-provider-anchor-policy-incompatibility-reason
          path anchor 'openai "other-model" fingerprints)
         'model-mismatch))
    (should
     (eq (e-session-provider-anchor-policy-incompatibility-reason
          '((:id "entry-1")) anchor 'openai "gpt-test" fingerprints)
         'anchor-not-on-current-path))))

(ert-deftest e-session-policy-test-board-routing-policy-is-copyable-and-normalized ()
  "Declarative routing policy validation owns detached value normalization."
  (let* ((policy
          '(:participant-id "participant"
            :pickup-selector (:kind "input"
                             :tags (main private)
                             :attributes (:source "test"))
            :observer-selector (:kind activity :tags-all (main))
            :default-tags (main)
            :default-to "participant"))
         (copy (e-session-board-routing-policy-copy-value policy))
         (normalized (e-session-board-routing-policy-normalize
                      (e-session-board-routing-policy-copy-value policy))))
    (should (e-session-board-routing-policy-valid-p policy))
    (should (equal copy policy))
    (should-not (eq copy policy))
    (should (eq (plist-get (plist-get normalized :pickup-selector) :kind)
                'input))
    (should (eq (car (plist-get normalized :default-tags)) 'main))
    (should-not
     (e-session-board-routing-policy-valid-p
      (plist-put (copy-tree policy) :unknown t)))
    (should-not
     (e-session-board-routing-policy-valid-p
      (plist-put (copy-tree policy) :pickup-selector
                 '(:kind input :predicate (lambda () t)))))))

(provide 'e-session-policy-test)

;;; e-session-policy-test.el ends here
