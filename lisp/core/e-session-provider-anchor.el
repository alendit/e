;;; e-session-provider-anchor.el --- Provider-anchor compatibility policy -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure compatibility policy over an explicit current-path projection.  It
;; never reaches into the session aggregate or persistence state.

;;; Code:

(require 'cl-lib)

(defun e-session-provider-anchor--keyword-plist-p (value)
  "Return non-nil when VALUE has keyword plist shape."
  (and (proper-list-p value)
       (let ((tail value)
             (valid t))
         (while (and valid tail)
           (setq valid
                 (and (consp tail)
                      (keywordp (car tail))
                      (consp (cdr tail))))
           (setq tail (cddr tail)))
         valid)))

(defun e-session-provider-anchor--dynamic-segment-p (segment)
  "Return non-nil when SEGMENT is volatile current-state context."
  (let ((kind (and (e-session-provider-anchor--keyword-plist-p segment)
                   (plist-get segment :kind))))
    (or (eq kind 'current-state)
        (eq kind 'dynamic-context)
        (equal kind "current-state")
        (equal kind "dynamic-context"))))

(defun e-session-provider-anchor--segment-list-p (segments)
  "Return non-nil when SEGMENTS is a list of segment plists."
  (and (proper-list-p segments)
       (cl-every (lambda (segment)
                   (and (e-session-provider-anchor--keyword-plist-p segment)
                        (plist-member segment :kind)))
                 segments)))

(defun e-session-provider-anchor--stable-segments (fingerprints)
  "Return provider-anchor hard-identity segments from FINGERPRINTS."
  (let ((segments (and (e-session-provider-anchor--keyword-plist-p fingerprints)
                       (plist-get fingerprints :segments))))
    (cond
     ((null segments) nil)
     ((e-session-provider-anchor--segment-list-p segments)
      (cl-remove-if
       #'e-session-provider-anchor--dynamic-segment-p
       segments))
     (t (list :invalid-provider-anchor-segments)))))

(defun e-session-provider-anchor-policy-incompatibility-reason
    (path anchor provider-id model fingerprints)
  "Return why ANCHOR is not compatible, or nil when compatible."
  (let* ((path-ids (mapcar (lambda (entry) (plist-get entry :id)) path))
         (anchor-id (plist-get anchor :id))
         (covered-entry-id (plist-get anchor :covered-entry-id))
         (anchor-fingerprints (plist-get anchor :fingerprints)))
    (cond
     ((not (eq (plist-get anchor :type) 'provider-anchor))
      'invalid-anchor-type)
     ((not (eq (plist-get anchor :provider-id) provider-id))
      'provider-mismatch)
     ((not (equal (plist-get anchor :model) model))
      'model-mismatch)
     ((not (equal (e-session-provider-anchor--stable-segments
                   anchor-fingerprints)
                  (e-session-provider-anchor--stable-segments
                   fingerprints)))
      'segment-fingerprint-mismatch)
     ((and (or (plist-member anchor-fingerprints :observation-delivery)
               (plist-member fingerprints :observation-delivery))
           (not (equal (plist-get anchor-fingerprints :observation-delivery)
                       (plist-get fingerprints :observation-delivery))))
      'observation-delivery-changed)
     ((and (or (plist-member anchor-fingerprints :current-state-fingerprint)
               (plist-member fingerprints :current-state-fingerprint))
           (not (equal
                 (plist-get anchor-fingerprints :current-state-fingerprint)
                 (plist-get fingerprints :current-state-fingerprint))))
      'current-state-changed)
     ((not (equal (plist-get anchor-fingerprints :active-layer-ids)
                  (plist-get fingerprints :active-layer-ids)))
      'active-layers-changed)
     ((not (equal (plist-get anchor-fingerprints :tools)
                  (plist-get fingerprints :tools)))
      'tools-changed)
     ((not (equal (plist-get anchor-fingerprints :reasoning)
                  (plist-get fingerprints :reasoning)))
      'reasoning-changed)
     ((not (equal (plist-get anchor-fingerprints :provider-options)
                  (plist-get fingerprints :provider-options)))
      'provider-options-changed)
     ((not (equal (plist-get anchor-fingerprints :compaction-boundary)
                  (plist-get fingerprints :compaction-boundary)))
      'compaction-boundary-changed)
     ((not (equal (plist-get anchor-fingerprints :lifetime-generation)
                  (plist-get fingerprints :lifetime-generation)))
      'context-generation-changed)
     ((and (or (plist-member anchor-fingerprints
                            :context-curation-revision-identity)
               (plist-member fingerprints
                            :context-curation-revision-identity))
           (not (equal
                 (plist-get anchor-fingerprints
                            :context-curation-revision-identity)
                 (plist-get fingerprints
                            :context-curation-revision-identity))))
      'context-curation-revision-changed)
     ((and (not (or (plist-member anchor-fingerprints :segments)
                    (plist-member anchor-fingerprints :active-layer-ids)
                    (plist-member anchor-fingerprints :tools)
                    (plist-member anchor-fingerprints :reasoning)
                    (plist-member anchor-fingerprints :provider-options)
                    (plist-member anchor-fingerprints :compaction-boundary)
                    (plist-member anchor-fingerprints :lifetime-generation)))
           (not (equal anchor-fingerprints fingerprints)))
      'fingerprint-mismatch)
     ((not (member anchor-id path-ids))
      'anchor-not-on-current-path)
     ((not (member covered-entry-id path-ids))
      'covered-entry-not-on-current-path)
     (t nil))))

(defun e-session-provider-anchor-policy-compatible-p
    (path anchor provider-id model fingerprints)
  "Return non-nil when ANCHOR is compatible with PATH."
  (null
   (e-session-provider-anchor-policy-incompatibility-reason
    path anchor provider-id model fingerprints)))


(provide 'e-session-provider-anchor)

;;; e-session-provider-anchor.el ends here
