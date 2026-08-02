;;; e-harness-instances.el --- User-facing harness instance catalog -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; User-facing catalog of configured runtime targets.  Live harness objects and
;; lazy factories remain owned by `e-harness-registry'; this module adds the
;; selection metadata shells need to present those targets uniformly.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-harness-registry)
(require 'e-request)

(define-error 'e-harness-instance-missing
  "No harness instance is registered for id")
(define-error 'e-harness-instance-store-conflict
  "Harness instances sharing a session store disagree on its ports")
(define-error 'e-harness-instance-session-store-missing
  "No configured harness instance exposes this session store")
(define-error 'e-harness-instance-session-catalog-invalid-page
  "Session catalog returned an invalid bounded page")
(define-error 'e-harness-instance-session-catalog-invalid-row
  "Session catalog returned a non-dormant or outdated session row")
(define-error 'e-harness-instance-session-request-stale
  "Harness instance catalog changed while a session request was pending")
(define-error 'e-harness-instance-session-catalog-stale
  "Harness instance catalog changed while a session page was pending"
  'e-harness-instance-session-request-stale)
(define-error 'e-harness-instance-session-access-invalid-operation
  "Unsupported session access-store operation")
(define-error 'e-harness-instance-session-access-invalid-result
  "Session access store returned an invalid current access record")
(define-error 'e-harness-instance-session-access-stale
  "Harness instance catalog changed while a session access request was pending"
  'e-harness-instance-session-request-stale)

(defconst e-harness-instance-session-access-operations
  '(create grant revoke transfer)
  "Closed set of optimistic session access-store mutations.")

(cl-defstruct e-harness-instance
  id
  name
  kind
  factory
  harness-id
  metadata
  default-p
  description
  (context-visibility 'always)
  subagent-p
  layers
  layer-config
  session-store-id
  session-catalog
  session-access-store)

(defvar e-harness-instance--instances (make-hash-table :test 'equal)
  "Harness instance records keyed by instance id.")

(defvar e-harness-instance--defaults (make-hash-table :test 'equal)
  "Default harness instance ids keyed by kind.")

(defvar e-harness-instance--session-stores (make-hash-table :test 'equal)
  "Configured session-store port records keyed by stable store id.")

(defvar e-harness-instance--generation 0
  "Monotonic generation of the configured harness-instance catalog.")

(defvar e-harness-instance--request-sequence 0
  "Process-local sequence for session catalog request identities.")

(defun e-harness-instance--validate-id (id)
  "Signal when ID is not a valid harness instance id."
  (unless (keywordp id)
    (signal 'wrong-type-argument (list 'keywordp id))))

(defun e-harness-instance--validate-kind (kind)
  "Signal when KIND is not a valid harness instance kind."
  (unless (symbolp kind)
    (signal 'wrong-type-argument (list 'symbolp kind))))

(defun e-harness-instance--validate-context-visibility (visibility)
  "Signal when VISIBILITY is not a valid context-visibility value."
  (unless (memq visibility '(always hidden))
    (signal 'wrong-type-argument
            (list '(member always hidden) visibility))))

(defun e-harness-instance--validate-session-port (name port)
  "Signal unless optional session NAME PORT is a callable application port."
  (when (and port (not (functionp port)))
    (signal 'wrong-type-argument (list 'functionp port))))

(defun e-harness-instance--validate-shared-store-ports
    (id session-store-id session-catalog session-access-store)
  "Require one stable pair of ports for every SESSION-STORE-ID.
ID's replacement registration is excluded so an instance can update its own
metadata without comparing against its retired record."
  (when-let ((entry (and session-store-id
                         (gethash session-store-id
                                  e-harness-instance--session-stores))))
    (when (and (seq-some (lambda (other-id) (not (eq other-id id)))
                         (plist-get entry :eligible-instance-ids))
               (not (and (eq (plist-get entry :session-catalog)
                             session-catalog)
                         (eq (plist-get entry :session-access-store)
                             session-access-store))))
      (signal 'e-harness-instance-store-conflict
              (list session-store-id id 'shared-store)))))

(defun e-harness-instance--unindex-session-store (instance)
  "Remove INSTANCE from its configured session-store eligibility index."
  (when-let* ((store-id (e-harness-instance-session-store-id instance))
              (entry (gethash store-id e-harness-instance--session-stores)))
    (let ((eligible (delq (e-harness-instance-id instance)
                          (copy-sequence
                           (plist-get entry :eligible-instance-ids)))))
      (if eligible
          (plist-put entry :eligible-instance-ids eligible)
        (remhash store-id e-harness-instance--session-stores)))))

(defun e-harness-instance--index-session-store (instance)
  "Add INSTANCE to its configured session-store eligibility index."
  (when-let ((store-id (e-harness-instance-session-store-id instance)))
    (let ((entry (gethash store-id e-harness-instance--session-stores)))
      (if entry
          (cl-pushnew (e-harness-instance-id instance)
                      (plist-get entry :eligible-instance-ids)
                      :test #'eq)
        (puthash store-id
                 (list :session-store-id store-id
                       :session-catalog
                       (e-harness-instance-session-catalog instance)
                       :session-access-store
                       (e-harness-instance-session-access-store instance)
                       :eligible-instance-ids
                       (list (e-harness-instance-id instance)))
                 e-harness-instance--session-stores)))))

(defun e-harness-instance--copy-session-store-entry (entry)
  "Copy mutable metadata in session-store ENTRY while preserving port identity."
  (list :session-store-id (plist-get entry :session-store-id)
        :session-catalog (plist-get entry :session-catalog)
        :session-access-store (plist-get entry :session-access-store)
        :eligible-instance-ids
        (sort (copy-sequence (plist-get entry :eligible-instance-ids))
              (lambda (left right)
                (string< (symbol-name left) (symbol-name right))))))

(defun e-harness-instance--display-name (id name)
  "Return normalized display NAME for ID."
  (cond
   ((and (stringp name) (not (string-empty-p name))) name)
   ((keywordp id) (string-remove-prefix ":" (symbol-name id)))
   (t (format "%s" id))))

;;;###autoload
(cl-defun e-harness-instance-register
    (&key id name kind factory harness-id metadata default
          description (context-visibility 'always) subagent
          layers layer-config session-store-id session-catalog session-access-store)
  "Register a configured harness instance.
ID is the stable user-facing target id.  KIND identifies the role the
instance plays, such as `chat' or `reviewer'.  FACTORY, when non-nil, is
registered with `e-harness-registry' under HARNESS-ID or ID.  METADATA is
presentation data.  When DEFAULT is non-nil, make this instance the default
for KIND.

DESCRIPTION is free-text \"when to use this agent\" routing guidance.
CONTEXT-VISIBILITY is `always' or `hidden' and controls whether the instance
appears in the subagents context block.  When SUBAGENT is non-nil, the
instance is spawnable as a subagent type; this eligibility flag is kept
separate from KIND so role instances stay reusable across chat and subagent
use.

LAYERS, when non-nil, is the instance's declared enabled layer id list; the
subagent runner applies it as the child harness's minimal layer set.
LAYER-CONFIG, when non-nil, is an alist mapping a capability id to its option
plist, applied as that instance's initial runtime capability config.  Both are
declarative selection metadata; the factory still builds the live harness."
  (e-harness-instance--validate-id id)
  (e-harness-instance--validate-kind kind)
  (e-harness-instance--validate-context-visibility context-visibility)
  (when (and session-store-id (not (stringp session-store-id)))
    (signal 'wrong-type-argument (list 'stringp session-store-id)))
  (e-harness-instance--validate-session-port 'session-catalog session-catalog)
  (e-harness-instance--validate-session-port 'session-access-store session-access-store)
  (when (and session-store-id (not (and session-catalog session-access-store)))
    (signal 'e-harness-instance-store-conflict
            (list session-store-id 'missing-required-port)))
  (e-harness-instance--validate-shared-store-ports
   id session-store-id session-catalog session-access-store)
  (let ((harness-id (or harness-id id))
        (previous (gethash id e-harness-instance--instances)))
    (e-harness-instance--validate-id harness-id)
    (when factory
      (e-harness-registry-register-factory harness-id factory))
    (let ((instance (make-e-harness-instance
                     :id id
                     :name (e-harness-instance--display-name id name)
                     :kind kind
                     :factory factory
                     :harness-id harness-id
                     :metadata metadata
                     :default-p default
                     :description description
                     :context-visibility context-visibility
                     :subagent-p subagent
                     :layers layers
                     :layer-config layer-config
                     :session-store-id session-store-id
                     :session-catalog session-catalog
                     :session-access-store session-access-store)))
      (when previous
        (e-harness-instance--unindex-session-store previous))
      (puthash id instance e-harness-instance--instances)
      (e-harness-instance--index-session-store instance)
      (cl-incf e-harness-instance--generation)
      (when (or default
                (not (gethash kind e-harness-instance--defaults)))
        (puthash kind id e-harness-instance--defaults))
      instance)))

(defun e-harness-instance-get (id)
  "Return registered harness instance ID, or nil."
  (e-harness-instance--validate-id id)
  (gethash id e-harness-instance--instances))

(defun e-harness-instance-generation ()
  "Return the current configured harness-instance catalog generation."
  e-harness-instance--generation)

(cl-defun e-harness-instance-list (&key kind)
  "Return registered harness instances, optionally filtered by KIND."
  (when kind
    (e-harness-instance--validate-kind kind))
  (let (instances)
    (maphash
     (lambda (_id instance)
       (when (or (not kind)
                 (eq (e-harness-instance-kind instance) kind))
         (push instance instances)))
     e-harness-instance--instances)
    (sort instances
          (lambda (left right)
            (string< (symbol-name (e-harness-instance-id left))
                     (symbol-name (e-harness-instance-id right)))))))

(defun e-harness-instance-session-stores ()
  "Return deduplicated configured store metadata without activating harnesses."
  (let (result)
    (maphash
     (lambda (_store-id indexed-entry)
       (push (e-harness-instance--copy-session-store-entry indexed-entry)
             result))
     e-harness-instance--session-stores)
    (sort result (lambda (left right)
                   (string< (plist-get left :session-store-id)
                            (plist-get right :session-store-id))))))

(defun e-harness-instance-session-store (session-store-id)
  "Return indexed metadata for SESSION-STORE-ID without activating a harness."
  (unless (stringp session-store-id)
    (signal 'wrong-type-argument (list 'stringp session-store-id)))
  (or (when-let ((entry (gethash session-store-id
                                  e-harness-instance--session-stores)))
        (e-harness-instance--copy-session-store-entry entry))
      (signal 'e-harness-instance-session-store-missing
              (list session-store-id))))

(defun e-harness-instance--normalize-session-catalog-page (entry page limit)
  "Validate and decorate one bounded catalog PAGE for indexed store ENTRY."
  (let ((sessions (and (listp page) (plist-get page :sessions))))
    (unless (and (listp page) (plist-member page :sessions))
      (signal 'e-harness-instance-session-catalog-invalid-page
              (list (plist-get entry :session-store-id) limit)))
    (let ((cursor sessions)
          (count 0))
      (while (and (consp cursor) (< count (1+ limit)))
        (unless (listp (car cursor))
          (signal 'e-harness-instance-session-catalog-invalid-page
                  (list (plist-get entry :session-store-id) limit)))
        (cl-incf count)
        (setq cursor (cdr cursor)))
      (unless (and (null cursor) (<= count limit))
        (signal 'e-harness-instance-session-catalog-invalid-page
                (list (plist-get entry :session-store-id) limit))))
    (list :session-store-id (plist-get entry :session-store-id)
          :eligible-instance-ids
          (copy-sequence (plist-get entry :eligible-instance-ids))
          :sessions
          (mapcar
           (lambda (session)
             (e-harness-instance--normalize-session-catalog-row
              entry session))
           sessions)
          :next-after (plist-get page :next-after))))

(defun e-harness-instance--current-session-access-record-p (record)
  "Return non-nil when RECORD has the required current access identity fields."
  (and (listp record)
       (plist-member record :controller)
       (plist-get record :controller)
       (plist-member record :version)
       (integerp (plist-get record :version))
       (>= (plist-get record :version) 0)
       (plist-member record :discover-principals)
       (listp (plist-get record :discover-principals))
       (plist-member record :resume-principals)
       (listp (plist-get record :resume-principals))))

(defun e-harness-instance--normalize-session-catalog-row
    (entry row &optional expected-session-id)
  "Validate and decorate one current dormant catalog ROW from store ENTRY."
  (let ((session-id (and (listp row) (plist-get row :session-id)))
        (access-record (and (listp row) (plist-get row :access-record)))
        (output-sequence (and (listp row)
                              (plist-get row :board-output-sequence)))
        (activity-sequence (and (listp row)
                                (plist-get row :board-activity-sequence))))
    (unless (and session-id
                 (or (null expected-session-id)
                     (equal session-id expected-session-id))
                 (eq (plist-get row :state) 'dormant)
                 (e-harness-instance--current-session-access-record-p access-record)
                 (integerp output-sequence) (>= output-sequence 0)
                 (integerp activity-sequence) (>= activity-sequence 0))
      (signal 'e-harness-instance-session-catalog-invalid-row
              (list (plist-get entry :session-store-id) session-id)))
    (append
     (list :session-store-id (plist-get entry :session-store-id)
           :eligible-instance-ids
           (copy-sequence (plist-get entry :eligible-instance-ids)))
     (copy-tree row))))

(cl-defun e-harness-instance--session-request-start
    (entry port-key owner id-prefix arguments normalizer stale-error
           &key on-done on-error)
  "Start one generation-fenced asynchronous session-store request.
ENTRY supplies PORT-KEY.  OWNER and ID-PREFIX identify the lifecycle.
ARGUMENTS is the immutable port request.  NORMALIZER validates the successful
adapter result.  STALE-ERROR identifies a completion fenced by reconfiguration."
  (let ((port (plist-get entry port-key))
        (generation e-harness-instance--generation)
        cancel
        request)
    (cl-labels
        ((fail (condition)
           (when (e-request-fail request condition)
             (when on-error
               (funcall on-error condition))))
         (finish (result)
           (unless (e-request-terminal-p request)
             (let (normalized normalized-p)
               (condition-case condition
                   (if (/= generation e-harness-instance--generation)
                       (signal stale-error
                               (list (plist-get entry :session-store-id)
                                     generation e-harness-instance--generation))
                     (setq normalized (funcall normalizer result)
                           normalized-p t))
                 (error
                  (fail condition)))
               (when (and normalized-p
                          (e-request-finish request normalized))
                 (when on-done
                   (funcall on-done normalized)))))))
      (setq request
            (e-request-lifecycle-create
             :id (format "%s-%d" id-prefix
                         (cl-incf e-harness-instance--request-sequence))
             :owner owner
             :generation generation
             :state 'created
             :cancel-function
             (lambda (_request)
               (when cancel
                 (funcall cancel)))))
      (e-request-start request arguments)
      (condition-case condition
          (let ((returned (funcall port arguments #'finish #'fail)))
            (when (and returned (not (functionp returned)))
              (signal 'wrong-type-argument (list 'functionp returned)))
            (setq cancel returned))
        (error
         (if (e-request-terminal-p request)
             (signal (car condition) (cdr condition))
           (fail condition))))
      request)))

(cl-defun e-harness-instance-session-catalog-page-start
    (session-store-id &key principal after (limit 32) on-done on-error)
  "Start one asynchronous dormant-session catalog page request.
SESSION-STORE-ID is resolved through the indexed configured-store map, without
calling a harness factory.  PRINCIPAL must come from the trusted host identity
boundary.  The catalog port receives one immutable request plist plus success
and failure callbacks; it may return a cancellation function.  The returned
`e-request-lifecycle' remains pending until one callback settles it."
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'plusp limit)))
  (let* ((entry (e-harness-instance-session-store session-store-id))
         (arguments (list :operation 'list-page
                          :session-store-id session-store-id
                          :principal principal
                          :after after
                          :limit limit)))
    (e-harness-instance--session-request-start
     entry :session-catalog 'e-harness-instance-session-catalog
     "session-catalog" arguments
     (lambda (page)
       (e-harness-instance--normalize-session-catalog-page entry page limit))
     'e-harness-instance-session-catalog-stale
     :on-done on-done :on-error on-error)))

(cl-defun e-harness-instance-session-catalog-read-start
    (session-store-id session-id &key principal on-done on-error)
  "Start one exact asynchronous dormant SESSION-ID catalog read.
The request uses only the configured store port and never invokes a harness
factory.  Successful completion requires a current dormant row with its
versioned access record and persisted board publication counters."
  (unless session-id
    (signal 'wrong-type-argument (list 'identity session-id)))
  (let* ((entry (e-harness-instance-session-store session-store-id))
         (arguments (list :operation 'read
                          :session-store-id session-store-id
                          :session-id session-id
                          :principal principal)))
    (e-harness-instance--session-request-start
     entry :session-catalog 'e-harness-instance-session-catalog
     "session-catalog-read" arguments
     (lambda (row)
       (e-harness-instance--normalize-session-catalog-row
        entry row session-id))
     'e-harness-instance-session-catalog-stale
     :on-done on-done :on-error on-error)))

(defun e-harness-instance--normalize-session-access-result (entry result)
  "Validate and decorate one access-store RESULT for configured store ENTRY."
  (let ((record (and (listp result) (plist-get result :access-record))))
    (unless (and (listp result)
                 (plist-member result :access-record)
                 (e-harness-instance--current-session-access-record-p record))
      (signal 'e-harness-instance-session-access-invalid-result
              (list (plist-get entry :session-store-id))))
    (append (list :session-store-id (plist-get entry :session-store-id))
            (copy-tree result))))

(cl-defun e-harness-instance-session-access-start
    (session-store-id operation arguments &key on-done on-error)
  "Start one optimistic asynchronous session access-store mutation.
OPERATION is one of `create', `grant', `revoke', or `transfer'.  ARGUMENTS must
name =:session-id=, a trusted =:requester-principal= resolved by the host, and an
explicit =:expected-version= (which may be nil for an absent-record create).
The configured port receives no transcript or live harness object."
  (unless (memq operation e-harness-instance-session-access-operations)
    (signal 'e-harness-instance-session-access-invalid-operation
            (list operation)))
  (unless (and (listp arguments)
               (plist-get arguments :session-id)
               (plist-get arguments :requester-principal)
               (plist-member arguments :expected-version))
    (signal 'wrong-type-argument
            (list 'session-access-arguments arguments)))
  (let* ((entry (e-harness-instance-session-store session-store-id))
         (request-arguments
          (append (list :operation operation
                        :session-store-id session-store-id)
                  (copy-tree arguments))))
    (e-harness-instance--session-request-start
     entry :session-access-store 'e-harness-instance-session-access
     "session-access" request-arguments
     (lambda (result)
       (e-harness-instance--normalize-session-access-result entry result))
     'e-harness-instance-session-access-stale
     :on-done on-done :on-error on-error)))

(cl-defun e-harness-instance-list-subagents (&key visibility)
  "Return spawnable subagent instances, optionally filtered by VISIBILITY.
VISIBILITY, when non-nil, is `always' or `hidden'."
  (when visibility
    (e-harness-instance--validate-context-visibility visibility))
  (seq-filter
   (lambda (instance)
     (and (e-harness-instance-subagent-p instance)
          (or (not visibility)
              (eq (e-harness-instance-context-visibility instance)
                  visibility))))
   (e-harness-instance-list)))

(cl-defun e-harness-instance-default (&key kind)
  "Return the default harness instance for KIND, or nil."
  (when kind
    (e-harness-instance--validate-kind kind))
  (or (when kind
        (when-let ((id (gethash kind e-harness-instance--defaults)))
          (let ((instance (e-harness-instance-get id)))
            (and instance
                 (eq (e-harness-instance-kind instance) kind)
                 instance))))
      (car (e-harness-instance-list :kind kind))))

(defun e-harness-instance-get-or-create (id)
  "Return the live harness for harness instance ID, creating it lazily."
  (e-harness-instance--validate-id id)
  (let ((instance (or (e-harness-instance-get id)
                      (signal 'e-harness-instance-missing (list id)))))
    (e-harness-registry-get-or-create
     (e-harness-instance-harness-id instance))))

(provide 'e-harness-instances)

;;; e-harness-instances.el ends here
