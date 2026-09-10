;;; e-board-sqlite-service.el --- SQL-owned Board application service -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Consumer-shaped asynchronous Board operations for ordinary SQLite
;; composition.  The service retains only the runtime transport.  Canonical
;; identities, order, routing, pickup state, and read cursors are returned by
;; the worker transaction and are never predicted or mirrored here.

;;; Code:

(require 'cl-lib)
(require 'e-board-orchestration)
(require 'e-board-storage)
(require 'e-runtime-store)
(require 'e-work)

(cl-defstruct (e-board-sqlite-service
               (:constructor e-board-sqlite-service--create))
  runtime)

(cl-defstruct (e-board-sqlite-service-operation
               (:constructor e-board-sqlite-service--operation-create))
  service kind body owner-key work request settled)

(defun e-board-sqlite-service-create (runtime)
  "Return a stateless Board application service over RUNTIME."
  (unless (e-runtime-store-p runtime)
    (signal 'wrong-type-argument (list 'e-runtime-store-p runtime)))
  (e-board-sqlite-service--create :runtime runtime))

(defun e-board-sqlite-service--settle (operation request)
  "Settle OPERATION exactly once from terminal runtime REQUEST."
  (unless (e-board-sqlite-service-operation-settled operation)
    (setf (e-board-sqlite-service-operation-settled operation) t
          (e-board-sqlite-service-operation-request operation) nil)
    (let ((work (e-board-sqlite-service-operation-work operation)))
      (if (eq (e-runtime-store-request--state request) 'committed)
          (e-work-finish
           work (copy-tree (e-runtime-store-request--result request) t))
        (e-work-fail
         work
         (or (copy-tree (e-runtime-store-request--error request) t)
             '(e-board-storage-error "Board operation did not commit")))))))

(defun e-board-sqlite-service--run (handle operation _context)
  "Submit OPERATION without waiting for worker open or acknowledgement."
  (let* ((service (e-board-sqlite-service-operation-service operation))
         (runtime (e-board-sqlite-service-runtime service))
         (kind (e-board-sqlite-service-operation-kind operation))
         (request
          (if-let* ((owner-key
                     (e-board-sqlite-service-operation-owner-key operation)))
              (e-runtime-store--submit-owned
               runtime kind (e-board-sqlite-service-operation-body operation)
               owner-key)
            (e-runtime-store-submit
             runtime kind (e-board-sqlite-service-operation-body operation)))))
    (setf (e-board-sqlite-service-operation-work operation) handle
          (e-work-handle-cancel-function handle)
          (lambda (_handle)
            (when-let* ((pending
                         (e-board-sqlite-service-operation-request operation)))
              (e-runtime-store-cancel runtime pending))))
    (e-runtime-store--observe
     request
     (lambda (settled)
       (e-board-sqlite-service--settle operation settled)))
    (unless (e-board-sqlite-service-operation-settled operation)
      (setf (e-board-sqlite-service-operation-request operation) request))
    :deferred))

(defconst e-board-sqlite-service--work-spec
  (e-work-spec-create
   :id "board-sql-operation" :execution 'cooperative
   :interactive-policy 'async :owner 'e-board-sqlite-service
   :runner #'e-board-sqlite-service--run))

(defun e-board-sqlite-service--start
    (service kind body &optional owner-key)
  "Start one request-scoped SERVICE KIND BODY operation."
  (unless (e-board-sqlite-service-p service)
    (signal 'wrong-type-argument (list 'e-board-sqlite-service-p service)))
  (let* ((operation
          (e-board-sqlite-service--operation-create
           :service service :kind kind :body (copy-tree body t)
           :owner-key (copy-tree owner-key t)))
         (work
          (e-work-prepare
           e-board-sqlite-service--work-spec operation
           :context
           (list :domain-ref (plist-get body :board-id)
                 :work-kind
                 (if (eq kind 'read) 'board-query 'board-mutation)))))
    (setf (e-board-sqlite-service-operation-work operation) work)
    (e-work-start-prepared work :arguments operation)
    work))

(cl-defun e-board-sqlite-service-append-route-start
    (service board-id &key session-id author requester-actor tags attributes to
             (mode 'inject) content reference source-input-key created-at)
  "Append and route one canonical input, returning request-scoped work."
  (unless (or (and (stringp board-id) (not (string-empty-p board-id)))
              (and (stringp session-id) (not (string-empty-p session-id))))
    (signal 'wrong-type-argument (list 'non-empty-string-p board-id)))
  (unless (and (stringp content) (not (string-empty-p content)))
    (user-error "Prompt must not be empty"))
  (unless source-input-key
    (signal 'e-board-storage-error
            (list "Board append requires a stable source identity" board-id)))
  (let ((signature
         (list :author author :requester-actor requester-actor
               :tags tags :attributes attributes :to to :mode mode
               :content content :reference reference)))
    (e-board-sqlite-service--start
     service 'write
     (list :op 'board-append-route :board-id board-id :session-id session-id
           :author author :requester-actor requester-actor
           :tags (copy-tree tags t) :attributes (copy-tree attributes t)
           :to (copy-tree to t) :mode mode :content (copy-sequence content)
           :reference (copy-tree reference t)
           :source-input-key (copy-tree source-input-key t)
           :source-hash (e-board-storage-signature-hash signature)
           :created-at created-at)
     (if board-id (cons 'board board-id) (cons 'session session-id)))))

(cl-defun e-board-sqlite-service-record-append-start
    (service board-id record-kind source-kind source-key &rest record-fields)
  "Append one canonical non-routed RECORD-KIND with detached RECORD-FIELDS."
  (let ((signature (list :record-kind record-kind
                         :record-fields record-fields)))
    (e-board-sqlite-service--start
     service 'write
     (list :op 'board-record-append :board-id board-id
           :record-kind record-kind :source-kind source-kind
           :source-key (copy-tree source-key t)
           :source-hash (e-board-storage-signature-hash signature)
           :record-fields (copy-tree record-fields t))
     (cons 'board board-id))))

(cl-defun e-board-sqlite-service-orchestration-fact-start
    (service board-id fact &key author)
  "Validate and append one orchestration FACT to BOARD-ID."
  (let ((fields
         (e-board-orchestration-fact-record-fields fact :author author)))
    (apply #'e-board-sqlite-service-record-append-start
           service board-id 'fact 'fact (plist-get fields :source-key)
           (cl-loop for (key value) on fields by #'cddr
                    unless (eq key :source-key)
                    append (list key value)))))

(defun e-board-sqlite-service-orchestration-run-start
    (service board-id run-id &optional limit)
  "Read one bounded durable RUN-ID fact set from BOARD-ID."
  (e-board-sqlite-service--start
   service 'read
   (list :op 'board-orchestration-run :board-id board-id
         :run-id run-id :limit (or limit 256))))

(defun e-board-sqlite-service-orchestration-runs-start
    (service board-id &optional limit)
  "Read facts for the newest bounded durable run set from BOARD-ID."
  (e-board-sqlite-service--start
   service 'read
   (list :op 'board-orchestration-runs :board-id board-id
         :limit (or limit 32))))

(defun e-board-sqlite-service-pickup-page-start
    (service board-id generation participant-id &optional limit)
  "Read PARTICIPANT-ID's bounded unresolved pickup page."
  (e-board-sqlite-service--start
   service 'read
   (list :op 'board-pickup-list :board-id board-id :generation generation
         :participant-id participant-id :limit (or limit 16))))

(defun e-board-sqlite-service-board-get-start (service board-id)
  "Read BOARD-ID's detached scalar coordination row."
  (e-board-sqlite-service--start
   service 'read (list :op 'board-get :board-id board-id)))

(cl-defun e-board-sqlite-service-admit-session-input-start
    (service session-id board-id principal records query-delta participant
             &key author requester-actor tags attributes to (mode 'inject)
             content reference source-input-key)
  "Atomically admit a new chat session and its first routed input."
  (let ((signature
         (list :author author :requester-actor requester-actor
               :tags tags :attributes attributes :to to :mode mode
               :content content :reference reference)))
    (e-board-sqlite-service--start
     service 'write
     (list :op 'chat-session-input-admit :session-id session-id
           :board-id board-id :principal principal
           :records (vconcat (copy-tree records t))
           :query-delta (copy-tree query-delta t)
           :participant (copy-tree participant t)
           :author author :requester-actor (copy-tree requester-actor t)
           :tags (copy-tree tags t) :attributes (copy-tree attributes t)
           :to (copy-tree to t) :mode mode :content (copy-sequence content)
           :reference (copy-tree reference t)
           :source-input-key (copy-tree source-input-key t)
           :source-hash (e-board-storage-signature-hash signature))
     (cons 'session session-id))))

(defun e-board-sqlite-service-admit-session-owner-start
    (service session-id board-id principal records query-delta participant)
  "Atomically admit a chat owner without creating a synthetic input row."
  (e-board-sqlite-service--start
   service 'write
   (list :op 'chat-session-owner-admit :session-id session-id
         :board-id board-id :principal principal
         :records (vconcat (copy-tree records t))
         :query-delta (copy-tree query-delta t)
         :participant (copy-tree participant t))
   (cons 'session session-id)))

(cl-defun e-board-sqlite-service-admit-participant-start
    (service session-id board-id records query-delta participant &key pickup)
  "Atomically admit SESSION-ID as BOARD-ID PARTICIPANT.

The worker resolves the Board's current generation inside the transaction;
the caller supplies no reconstructed Board or locally retained generation."
  (e-board-sqlite-service--start
   service 'write
   (append
    (list :op 'session-board-participant-admit
          :session-id session-id :board-id board-id
          :records (vconcat (copy-tree records t))
          :query-delta (copy-tree query-delta t)
          :participant (copy-tree participant t))
    (when pickup (list :pickup (copy-tree pickup t))))
   (cons 'session session-id)))

(defun e-board-sqlite-service-transition-pickup-start
    (service board-id delivery-id transition &optional data)
  "Start DELIVERY-ID TRANSITION and return canonical settlement work."
  (e-board-sqlite-service--start
   service 'write
   (list :op 'board-pickup-transition :board-id board-id
         :delivery-id (copy-tree delivery-id t)
         :transition transition :data (copy-tree data t))
   (cons 'board board-id)))

(cl-defun e-board-sqlite-service-record-page-start
    (service board-id &key generation after limit selector through)
  "Read one bounded canonical page and its SQLite snapshot cursor."
  (e-board-sqlite-service--start
   service 'read
   (list :op 'board-record-page :board-id board-id :generation generation
         :after after :limit limit :selector (copy-tree selector t)
         :through through)))

(cl-defun e-board-sqlite-service-visible-window-start
    (service board-id &key generation limit)
  "Read the recent visible Board window and its SQLite change boundary."
  (e-board-sqlite-service--start
   service 'read
   (list :op 'board-visible-window :board-id board-id
         :generation generation :limit (or limit 64))))

(provide 'e-board-sqlite-service)

;;; e-board-sqlite-service.el ends here
