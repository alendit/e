;;; e-resource-coherence.el --- Generic linked-resource coherence model -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Resource coherence models resources that are aliases, live views, or
;; persisted forms of the same underlying entity.  Concrete adapters contribute
;; providers that can describe a coherence group for URIs they understand.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-resources)

(define-error 'e-resource-coherence-conflict
  "Resource coherence conflict")
(define-error 'e-resource-coherence-unsupported
  "Resource coherence is not supported for this URI")

(cl-defstruct (e-resource-coherence-registry
               (:constructor e-resource-coherence-registry-create))
  (providers nil))

(cl-defstruct (e-resource-coherence-provider
               (:constructor e-resource-coherence-provider-create))
  id
  schemes
  handler)

(defun e-resource-coherence-register (registry provider)
  "Register coherence PROVIDER in REGISTRY."
  (unless (e-resource-coherence-provider-p provider)
    (signal 'wrong-type-argument
            (list 'e-resource-coherence-provider-p provider)))
  (setf (e-resource-coherence-registry-providers registry)
        (append (e-resource-coherence-registry-providers registry)
                (list provider)))
  provider)

(defun e-resource-coherence-view-create (&rest plist)
  "Return a resource coherence view from PLIST.
Important keys include:

`:uri'               view URI;
`:canonical-uri'     canonical URI for the underlying entity;
`:kind'              adapter-specific kind, such as `file' or `buffer';
`:role'              semantic role, such as `persisted' or `live-view';
`:status'            `coherent', `needs-save', `stale', `missing', or `unknown';
`:modified'          non-nil when the view has unsaved local changes;
`:live'              non-nil when the view is live process state;
`:visible'           non-nil when the view is user-visible;
`:selected-window'   non-nil when the view is selected/active;
`:priority'          numeric preference within otherwise equivalent views;
`:metadata'          adapter-specific details."
  plist)

(defun e-resource-coherence-group-create (&rest plist)
  "Return a resource coherence group from PLIST.
Expected keys are `:canonical-uri', `:subject-uri', `:views', and optional
`:metadata'."
  plist)

(defun e-resource-coherence--provider-supports-scheme-p (provider scheme)
  "Return non-nil when PROVIDER supports SCHEME."
  (member scheme (e-resource-coherence-provider-schemes provider)))

(defun e-resource-coherence--provider-group (provider parsed-uri)
  "Return PROVIDER group for PARSED-URI, or nil."
  (let ((handler (e-resource-coherence-provider-handler provider)))
    (unless (functionp handler)
      (signal 'wrong-type-argument (list 'functionp handler)))
    (funcall handler parsed-uri)))

(defun e-resource-coherence-group (registry uri)
  "Return coherence group for URI in REGISTRY, or signal unsupported."
  (let* ((parsed-uri (e-resources-parse-uri uri))
         (scheme (plist-get parsed-uri :scheme))
         (providers (seq-filter
                     (lambda (provider)
                       (e-resource-coherence--provider-supports-scheme-p
                        provider scheme))
                     (e-resource-coherence-registry-providers registry)))
         group)
    (while (and providers (not group))
      (setq group (e-resource-coherence--provider-group (pop providers)
                                                        parsed-uri)))
    (unless group
      (signal 'e-resource-coherence-unsupported
              (list (format "No resource coherence provider for URI: %s" uri))))
    group))

(defun e-resource-coherence-view-status (view)
  "Return VIEW coherence status."
  (plist-get view :status))

(defun e-resource-coherence-view-uri (view)
  "Return VIEW URI."
  (plist-get view :uri))

(defun e-resource-coherence-view-kind (view)
  "Return VIEW kind."
  (plist-get view :kind))

(defun e-resource-coherence-view-modified-p (view)
  "Return non-nil when VIEW is modified."
  (plist-get view :modified))

(defun e-resource-coherence-view-dirty-p (view)
  "Return non-nil when VIEW has local unsaved changes."
  (or (e-resource-coherence-view-modified-p view)
      (eq (e-resource-coherence-view-status view) 'needs-save)))

(defun e-resource-coherence-group-views (group)
  "Return views in GROUP."
  (plist-get group :views))

(defun e-resource-coherence-group-status (group)
  "Return aggregate coherence status for GROUP."
  (let ((statuses (mapcar #'e-resource-coherence-view-status
                          (e-resource-coherence-group-views group))))
    (cond
     ((memq 'needs-save statuses) 'needs-save)
     ((memq 'conflict statuses) 'conflict)
     ((memq 'stale statuses) 'stale)
     ((memq 'unknown statuses) 'unknown)
     ((memq 'missing statuses) 'missing)
     ((or (null statuses) (memq 'coherent statuses)) 'coherent)
     (t (car statuses)))))

(defun e-resource-coherence-group-with-status (group)
  "Return GROUP with aggregate `:status' populated."
  (plist-put (copy-sequence group)
             :status
             (e-resource-coherence-group-status group)))

(defun e-resource-coherence-dirty-views (group &optional except-uri)
  "Return dirty views in GROUP, optionally excluding EXCEPT-URI."
  (seq-filter (lambda (view)
                (and (e-resource-coherence-view-dirty-p view)
                     (not (equal (e-resource-coherence-view-uri view)
                                 except-uri))))
              (e-resource-coherence-group-views group)))

(defun e-resource-coherence--view-label (view)
  "Return a readable label for VIEW."
  (or (plist-get view :label)
      (e-resource-coherence-view-uri view)
      (format "%S" view)))

(defun e-resource-coherence-view-labels (views)
  "Return comma-separated labels for VIEWS."
  (mapconcat #'e-resource-coherence--view-label views ", "))

(defun e-resource-coherence-conflict-if-dirty (group &optional subject-uri action)
  "Signal when GROUP has dirty linked views.
SUBJECT-URI is omitted from dirty-view consideration.  ACTION is a verb used in
error text, defaulting to the edit action."
  (when-let ((dirty (e-resource-coherence-dirty-views group subject-uri)))
    (signal 'e-resource-coherence-conflict
            (list (format "Cannot %s %s directly because linked resource(s) %s have unsaved changes. Target the dirty resource explicitly or ask the user how to resolve the conflict."
                          (or action "edit")
                          (or subject-uri
                              (plist-get group :subject-uri)
                              (plist-get group :canonical-uri)
                              "resource")
                          (e-resource-coherence-view-labels dirty))))))

(defun e-resource-coherence--view-priority (view)
  "Return selection priority for VIEW."
  (cond
   ((plist-get view :selected-window) 300)
   ((plist-get view :visible) 200)
   ((plist-get view :live) 100)
   (t 0)))

(defun e-resource-coherence-preferred-view (views &optional description)
  "Return preferred view from VIEWS, or nil.
If multiple VIEWS are equally preferred, signal a coherence conflict.
DESCRIPTION is used in ambiguity error text."
  (when views
    (let* ((ranked (sort (copy-sequence views)
                         (lambda (left right)
                           (> (+ (e-resource-coherence--view-priority left)
                                 (or (plist-get left :priority) 0))
                              (+ (e-resource-coherence--view-priority right)
                                 (or (plist-get right :priority) 0))))))
           (best (car ranked))
           (best-score (+ (e-resource-coherence--view-priority best)
                          (or (plist-get best :priority) 0)))
           (ties (seq-take-while
                  (lambda (view)
                    (= (+ (e-resource-coherence--view-priority view)
                          (or (plist-get view :priority) 0))
                       best-score))
                  ranked)))
      (if (= (length ties) 1)
          best
        (signal 'e-resource-coherence-conflict
                (list (format "Cannot choose a preferred %s because multiple linked resources are equally preferred: %s. Target one resource explicitly or resolve the ambiguity."
                              (or description "resource view")
                              (e-resource-coherence-view-labels ties))))))))

(defun e-resource-coherence-filter-views (group predicate)
  "Return GROUP views matching PREDICATE."
  (seq-filter predicate (e-resource-coherence-group-views group)))

(defun e-resource-coherence-views-by-kind (group kind)
  "Return GROUP views whose `:kind' equals KIND."
  (e-resource-coherence-filter-views
   group
   (lambda (view)
     (eq (e-resource-coherence-view-kind view) kind))))

(provide 'e-resource-coherence)

;;; e-resource-coherence.el ends here
