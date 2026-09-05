;;; e-session-async.el --- Optimistic session persistence service -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The session application service admits each prevalidated aggregate mutation
;; directly to the runtime store's one FIFO.  Once enqueue succeeds, it installs
;; the aggregate-owned token immediately, so all later session readers observe
;; the same effective aggregate while durability is pending.  A terminal
;; runtime observation retires pending state; it never republishes the delta.

;;; Code:

(require 'cl-lib)
(require 'e-session-aggregate)
(require 'e-session-storage)
(require 'e-work)

(define-error 'e-session-async-capacity-exhausted
  "Session durable-operation capacity is exhausted"
  'e-session-storage-error)

(defconst e-session-async-suspect-diagnostic-byte-limit 1024)

(defvar e-session-async--install-fault-function nil
  "Optional test-only function called at pending/install admission edges.")

(cl-defstruct (e-session-async--state
               (:constructor e-session-async--state-create))
  store
  (pending (make-hash-table :test 'equal))
  (suspects (make-hash-table :test 'equal)))

(cl-defstruct (e-session-async--operation
               (:constructor e-session-async--operation-create))
  state session-id token work storage-operation write-index settled)

(defvar e-session-async--states
  (make-hash-table :test 'eq :weakness 'key)
  "Application-owned optimistic state keyed by session store.")

(defvar e-session-async--enabled-stores
  (make-hash-table :test 'eq :weakness 'key)
  "Persistent stores whose facades use optimistic asynchronous persistence.")

(defconst e-session-async--operation-spec
  (e-work-spec-create
   :id "session-durable" :execution 'cooperative :interactive-policy 'async
   :owner 'e-session-async
   :runner (lambda (_handle _operation _context) :deferred)))

(defun e-session-async--state (store)
  "Return STORE's application state, creating it when enabled."
  (or (gethash store e-session-async--states)
      (let ((state (e-session-async--state-create :store store)))
        (puthash store state e-session-async--states)
        state)))

(defun e-session-async-enable (store)
  "Enable optimistic asynchronous session persistence for STORE."
  (unless (e-session-storage-runtime-store store)
    (signal 'e-session-storage-error
            (list "Persistent session runtime is unavailable" store)))
  (puthash store t e-session-async--enabled-stores)
  (e-session-async--state store)
  store)

(defun e-session-async-enabled-p (store)
  "Return non-nil when STORE uses optimistic asynchronous persistence."
  (and (gethash store e-session-async--enabled-stores) t))

(defun e-session-async-pending-count (store session-id)
  "Return STORE's admitted unsettled mutation count for SESSION-ID."
  (if-let ((state (gethash store e-session-async--states)))
      (length (gethash session-id (e-session-async--state-pending state)))
    0))

(defun e-session-async-pending-p (store session-id)
  "Return non-nil when SESSION-ID has an admitted unsettled mutation."
  (> (e-session-async-pending-count store session-id) 0))

(defun e-session-async--utf8-prefix (string byte-limit)
  "Return STRING's longest prefix occupying at most BYTE-LIMIT UTF-8 bytes."
  (if (<= (string-bytes string) byte-limit)
      string
    (let ((low 0) (high (min (length string) byte-limit)))
      (while (< low high)
        (let ((middle (/ (+ low high 1) 2)))
          (if (<= (string-bytes (substring string 0 middle)) byte-limit)
              (setq low middle)
            (setq high (1- middle)))))
      (substring string 0 low))))

(defun e-session-async--detach-error (session-id error)
  "Return ERROR's bounded detached session-owned first-cause shape."
  (let* ((type (if (and (consp error) (symbolp (car error))
                        (get (car error) 'error-conditions))
                   (car error)
                 'e-session-storage-error))
         (properties (and (consp error) (cddr error)))
         (diagnostic
          (e-session-async--utf8-prefix
           (let ((print-circle t) (print-level 6) (print-length 32))
             (condition-case nil
                 (error-message-string error)
               (error "Session persistence failed")))
           e-session-async-suspect-diagnostic-byte-limit)))
    (list type (copy-sequence diagnostic)
          :session-id (and (stringp session-id) (copy-sequence session-id))
          :operation (plist-get properties :operation)
          :kind (plist-get properties :kind)
          :request-id
          (when-let ((request-id (plist-get properties :request-id)))
            (if (stringp request-id) (copy-sequence request-id) request-id)))))

(defun e-session-async--note-suspect (store session-id error)
  "Retain SESSION-ID's first bounded detached persistence ERROR in STORE."
  (let* ((state (e-session-async--state store))
         (suspects (e-session-async--state-suspects state)))
    (or (gethash session-id suspects)
        (let ((detached (e-session-async--detach-error session-id error)))
          (puthash (copy-sequence session-id) detached suspects)
          detached))))

(defun e-session-async-session-suspect (store session-id)
  "Return SESSION-ID's detached process-local persistence suspicion, or nil."
  (when-let* ((state (gethash store e-session-async--states))
              (status (gethash session-id
                               (e-session-async--state-suspects state))))
    (copy-tree status)))

(defun e-session-async--failed-work (session-id cause)
  "Return a terminal session work handle carrying typed CAUSE."
  (let ((work (e-work-prepare
               e-session-async--operation-spec nil
               :context (list :domain-ref session-id
                              :work-kind 'session-durable))))
    (e-work-fail work cause)
    work))

(defun e-session-async--start-work (session-id operation)
  "Return started deferred work for SESSION-ID owned by OPERATION."
  (let ((work (e-work-prepare
               e-session-async--operation-spec operation
               :context (list :domain-ref session-id
                              :work-kind 'session-durable))))
    (setf (e-session-async--operation-work operation) work)
    (e-work-start-prepared work :arguments operation)
    work))

(defun e-session-async--add-pending (operation)
  "Publish OPERATION in its session-owned pending set."
  (let* ((state (e-session-async--operation-state operation))
         (session-id (e-session-async--operation-session-id operation))
         (pending (e-session-async--state-pending state)))
    (e-session-async--install-fault 'pending-registration)
    (puthash session-id
             (cons operation (gethash session-id pending))
             pending)))

(defun e-session-async--remove-pending (operation)
  "Remove OPERATION once from its session-owned pending set."
  (when-let* ((state (e-session-async--operation-state operation))
              (session-id (e-session-async--operation-session-id operation)))
    (let* ((pending (e-session-async--state-pending state))
           (remaining (delq operation (gethash session-id pending))))
      (if remaining
          (puthash session-id remaining pending)
        (remhash session-id pending)))))

(defun e-session-async--install-fault (edge)
  "Invoke the test-only aggregate install fault at EDGE."
  (when e-session-async--install-fault-function
    (funcall e-session-async--install-fault-function edge)))

(defun e-session-async--fail-work-isolated (work cause)
  "Fail WORK with CAUSE without allowing arbitrary observers to gate cleanup."
  (condition-case nil
      (e-work-fail work cause)
    ((error quit) nil))
  work)

(defun e-session-async--operation-body (command delta before-submit)
  "Return COMMAND DELTA's physical body, augmented by BEFORE-SUBMIT."
  (let* ((session-id (e-session-aggregate-command-session-id command))
         (record (plist-get delta :record))
         (continuity (and before-submit (funcall before-submit delta))))
    (cond
     ((eq (e-session-aggregate-command-tag command) 'delete)
      (list :op 'session-delete :session-id session-id))
     (continuity
      (list :op 'session-append-with-tool-transition
            :session-id session-id :record record :continuity continuity))
     (t
      (list :op 'session-append :session-id session-id :record record)))))

(defun e-session-async--settle (operation result error)
  "Retire OPERATION authoritatively before notifying its public work."
  (unless (e-session-async--operation-settled operation)
    (setf (e-session-async--operation-settled operation) t)
    (e-session-async--remove-pending operation)
    (let* ((state (e-session-async--operation-state operation))
           (store (and state (e-session-async--state-store state)))
           (session-id (e-session-async--operation-session-id operation))
           (token (e-session-async--operation-token operation))
           (work (e-session-async--operation-work operation)))
      ;; Drop all physical/install ownership before arbitrary work callbacks.
      (setf (e-session-async--operation-storage-operation operation) nil
            (e-session-async--operation-token operation) nil)
      (if error
          (let ((cause (e-session-async--note-suspect store session-id error)))
            (e-work-fail work (copy-tree cause)))
        (when (and (e-session-aggregate-install-token-installed token)
                   (e-session-async--operation-write-index operation)
                   (plist-get (e-session-aggregate-install-token-delta token)
                              :record))
          (condition-case projection-error
              (e-session-storage--mark-checkpoint-dirty store session-id)
            ((error quit)
             (condition-case nil
                 (e-session-storage-note-projection-error store projection-error)
               ((error quit) nil)))))
        (e-work-finish work
                       (e-session-aggregate-install-token-result token))))))

(defun e-session-async--cancel-preinstall
    (operation cause &optional submitted-operation)
  "Cancel OPERATION after failed install and settle it with CAUSE."
  (let* ((state (e-session-async--operation-state operation))
         (store (e-session-async--state-store state))
         (session-id (e-session-async--operation-session-id operation))
         (storage-operation
          (or submitted-operation
              (e-session-async--operation-storage-operation operation)))
         (disposition
          (condition-case cancel-error
              (e-session-storage-cancel-operation store storage-operation)
            ((error quit) cancel-error))))
    (if (eq disposition 'dropped)
        (progn
          (setf (e-session-async--operation-settled operation) t)
          (e-session-async--remove-pending operation)
          (setf (e-session-async--operation-storage-operation operation) nil
                (e-session-async--operation-token operation) nil)
          (e-session-async--fail-work-isolated
           (e-session-async--operation-work operation) cause))
      ;; A request that may have crossed the worker boundary cannot be undone.
      ;; Preserve its pending ownership through the eventual runtime callback.
      (e-session-async--note-suspect store session-id cause)
      (e-session-async--fail-work-isolated
       (e-session-async--operation-work operation)
       (e-session-async--detach-error session-id cause)))))

(cl-defun e-session-async-submit-command
    (store session-id tag arguments &key before-submit write-index)
  "Admit one aggregate TAG directly to STORE's runtime FIFO.

The aggregate token and exact physical body are validated before enqueue.
Successful enqueue is followed, without yielding, by one immediate effective
aggregate install.  A later ACK only retires pending state and finishes the
returned work; it never reapplies the mutation."
  (condition-case err
      (let* ((command (e-session-aggregate-command-prepare
                       tag session-id arguments))
             (effective-id (e-session-aggregate-command-session-id command))
             (token
              (e-session-aggregate-prepare-install-token
               store command
               (lambda (delta)
                 (e-session-async--operation-body
                  command delta before-submit))))
             (body (e-session-aggregate-install-token-body token)))
        (if (null body)
            ;; Semantic no-ops own no runtime request or pending publication.
            (let* ((operation (e-session-async--operation-create
                               :state (e-session-async--state store)
                               :session-id effective-id :token token))
                   (work (e-session-async--start-work effective-id operation)))
              (condition-case install-error
                  (e-work-finish
                   work (e-session-aggregate-apply-install-token store token))
                ((error quit) (e-work-fail work install-error)))
              work)
          ;; Interpretation above is pure and is required to distinguish a
          ;; semantic no-op from a physical mutation.  Only the latter is
          ;; fenced for a suspect owner.
          (if-let ((suspect (e-session-async-session-suspect
                             store effective-id)))
              (e-session-async--failed-work effective-id suspect)
            (e-session-storage-validate-operation-body store body)
          (let* ((state (e-session-async--state store))
                 (operation (e-session-async--operation-create
                             :state state :session-id effective-id :token token
                             :write-index write-index))
                 (work (e-session-async--start-work effective-id operation))
                 submitted-operation)
            (condition-case admission-error
                (progn
                  ;; Allocate/publish pending ownership before the runtime
                  ;; accepts anything.  A failure here has no physical request
                  ;; to cancel and leaves no projection.
                  (e-session-async--add-pending operation)
                  (setq submitted-operation
                        (e-session-storage-submit-owned
                         store effective-id body
                         (lambda (result error)
                           (e-session-async--settle operation result error))))
                  (unless submitted-operation
                    (signal 'e-session-storage-error
                            (list "Session storage rejected an admitted write")))
                  ;; This non-yielding pointer transfer makes any later fault
                  ;; cancellable through the exact accepted operation.
                  (let ((inhibit-quit t))
                    (setf (e-session-async--operation-storage-operation operation)
                          submitted-operation))
                  (condition-case install-error
                      (progn
                        (e-session-async--install-fault 'before-install)
                        (e-session-aggregate-apply-install-token store token)
                        (e-session-async--install-fault 'after-install))
                    ((error quit)
                     (if (e-session-aggregate-install-token-installed token)
                         (progn
                           (e-session-async--note-suspect
                            store effective-id install-error)
                           (e-work-fail
                            work
                            (e-session-async--detach-error
                             effective-id install-error)))
                       (e-session-async--cancel-preinstall
                        operation install-error submitted-operation))))
                  work)
              (e-session-storage-admission-ambiguous
               (let* ((properties (cddr admission-error))
                      (ambiguous-operation
                       (plist-get properties :storage-operation)))
                 (let ((inhibit-quit t))
                   (setf (e-session-async--operation-storage-operation operation)
                         ambiguous-operation))
                 (e-session-async--note-suspect
                  store effective-id admission-error)
                 (e-session-async--fail-work-isolated
                  work
                  (e-session-async--detach-error
                   effective-id admission-error))
                 work))
              ((error quit)
               (if submitted-operation
                   (e-session-async--cancel-preinstall
                    operation admission-error submitted-operation)
                 (e-session-async--remove-pending operation))
               (unless (e-request-terminal-p (e-work-handle-lifecycle work))
                 (e-session-async--fail-work-isolated work admission-error))
               work))))))
    ((e-session-command-too-large e-runtime-store-codec-too-large
                                  e-runtime-store-request-too-large)
     (e-session-async--failed-work
      session-id
      (list 'e-session-async-capacity-exhausted
            "Session command exceeds its practical capacity" :cause err)))
    ((e-session-error e-session-board-message-invalid-record-type
                      e-context-lifetime-invalid-record wrong-type-argument)
     (e-session-async--failed-work
      session-id
     (list 'e-session-storage-command-error
            "Invalid asynchronous session command" :cause err)))))

(defun e-session-async-unsupported-command (session-id name)
  "Return a terminal typed work for unsupported asynchronous command NAME."
  (e-session-async--failed-work
   session-id
   (list 'e-session-storage-command-error
         "Unsupported asynchronous session command" name)))

(defun e-session-async-reset (store)
  "Clear STORE's process-local pending and suspect session state."
  (when-let ((state (gethash store e-session-async--states)))
    (let (operations)
      ;; Detach every application-owned link before arbitrary work observers.
      (unwind-protect
          (maphash (lambda (_session-id pending)
                     (setq operations (nconc pending operations)))
                   (e-session-async--state-pending state))
        (clrhash (e-session-async--state-pending state))
        (clrhash (e-session-async--state-suspects state)))
      (dolist (operation operations)
        (unless (e-session-async--operation-settled operation)
          (setf (e-session-async--operation-settled operation) t
                (e-session-async--operation-storage-operation operation) nil
                (e-session-async--operation-token operation) nil
                (e-session-async--operation-state operation) nil)
          (e-session-async--fail-work-isolated
           (e-session-async--operation-work operation)
           '(e-session-storage-error "Session store reset"))))))
  store)

(defun e-session-async-teardown (store)
  "Clear STORE's optimistic session state during close or reset."
  (e-session-async-reset store)
  (remhash store e-session-async--states)
  t)

(provide 'e-session-async)

;;; e-session-async.el ends here
