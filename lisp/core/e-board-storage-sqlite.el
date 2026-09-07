;;; e-board-storage-sqlite.el --- SQLite Board storage adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Maps the Board-owned storage port onto the existing typed runtime store.
;; SQL, schema, and composite transaction details remain worker-side.

;;; Code:

(require 'e-board-storage)
(require 'e-runtime-store)
(require 'e-work)

(defconst e-board-storage-sqlite--diagnostic-byte-limit 1024)

(cl-defstruct (e-board-storage-sqlite--query
               (:constructor e-board-storage-sqlite--query-create))
  "One request-local bounded Board query."
  runtime body work request settled)

(defun e-board-storage-sqlite--settle-query (query request)
  "Settle QUERY exactly once from terminal runtime REQUEST."
  (unless (e-board-storage-sqlite--query-settled query)
    (setf (e-board-storage-sqlite--query-settled query) t
          (e-board-storage-sqlite--query-request query) nil)
    (let ((work (e-board-storage-sqlite--query-work query)))
      (if (eq (e-runtime-store-request--state request) 'committed)
          (e-work-finish work
                         (copy-tree
                          (e-runtime-store-request--result request) t))
        (e-work-fail
         work
         (or (copy-tree (e-runtime-store-request--error request) t)
             '(e-board-storage-error "Board query did not commit")))))))

(defun e-board-storage-sqlite--run-query (handle query _context)
  "Submit request-local QUERY without waiting for its worker."
  (let* ((runtime (e-board-storage-sqlite--query-runtime query))
         (request
          (e-runtime-store-submit
           runtime 'read (e-board-storage-sqlite--query-body query))))
    (setf (e-board-storage-sqlite--query-work query) handle
          (e-work-handle-cancel-function handle)
          (lambda (_handle)
            (when-let* ((pending
                         (e-board-storage-sqlite--query-request query)))
              (e-runtime-store-cancel runtime pending))))
    (e-runtime-store--observe
     request (lambda (settled)
               (e-board-storage-sqlite--settle-query query settled)))
    (unless (e-board-storage-sqlite--query-settled query)
      (setf (e-board-storage-sqlite--query-request query) request))
    :deferred))

(defconst e-board-storage-sqlite--query-spec
  (e-work-spec-create
   :id "board-query" :execution 'cooperative :interactive-policy 'async
   :owner 'e-board-storage-sqlite
   :runner #'e-board-storage-sqlite--run-query))

(defun e-board-storage-sqlite-controller-state-start
    (runtime board-id &optional record-limit)
  "Return immediately with bounded detached controller state for BOARD-ID."
  (let* ((query
          (e-board-storage-sqlite--query-create
           :runtime runtime
           :body (list :op 'board-controller-state :board-id board-id
                       :record-limit (or record-limit 64))))
         (work
          (e-work-prepare
           e-board-storage-sqlite--query-spec query
           :context (list :domain-ref board-id :work-kind 'board-query))))
    (setf (e-board-storage-sqlite--query-work query) work)
    (e-work-start-prepared work :arguments query)
    work))

(defun e-board-storage-sqlite--utf8-prefix (string limit)
  "Return STRING truncated to at most LIMIT UTF-8 bytes."
  (if (<= (string-bytes string) limit)
      (copy-sequence string)
    (let ((low 0) (high (length string)))
      (while (< low high)
        (let ((mid (/ (+ low high 1) 2)))
          (if (<= (string-bytes (substring string 0 mid)) limit)
              (setq low mid)
            (setq high (1- mid)))))
      (substring string 0 low))))

(defun e-board-storage-sqlite--detached-error (error board-id)
  "Return bounded detached ERROR status for BOARD-ID."
  (let ((print-circle t) (print-level 6) (print-length 32))
    (list (if (and (consp error) (symbolp (car error)))
              (car error)
            'e-board-storage-error)
          (e-board-storage-sqlite--utf8-prefix
           (condition-case nil (error-message-string error)
             (error "Board persistence failed"))
           e-board-storage-sqlite--diagnostic-byte-limit)
          :board-id (copy-sequence board-id))))

(defun e-board-storage-sqlite--async-submit (storage board-id body result)
  "Submit BOARD-ID BODY and immediately return optimistic RESULT."
  (let* ((runtime (e-board-storage--runtime storage))
         (request (e-runtime-store--submit-owned
                   runtime 'write body (cons 'board board-id))))
    (cl-incf (e-board-storage--pending-count storage))
    (e-runtime-store--observe
     request
     (lambda (settled)
       (cl-decf (e-board-storage--pending-count storage))
       (let ((error
              (unless (eq (e-runtime-store-request--state settled) 'committed)
                (e-board-storage-sqlite--detached-error
                 (or (e-runtime-store-request--error settled)
                     '(e-board-storage-error "Board persistence did not commit"))
                 board-id))))
         (when (and error (null (e-board-storage--first-error storage)))
           (setf (e-board-storage--first-error storage) error))
         (when-let* ((observer (e-board-storage--settlement-function storage)))
           (funcall observer storage result error)))))
    result))

(defun e-board-storage-sqlite--async-call (storage operation arguments)
  "Dispatch asynchronous Board OPERATION with optimistic local projection."
  (pcase operation
    ('create-board
     (pcase-let ((`(,board-id ,principal ,root) arguments))
       (let ((result (list :board-id board-id :trusted-principal principal
                           :generation 1 :revision 1 :status 'created)))
         (e-board-storage-sqlite--async-submit
          storage board-id
          (list :op 'board-create :board-id board-id
                :trusted-principal principal :root root)
          result)
         (setf (e-board-storage--next-revision storage) 1)
         result)))
    ('clear-board
     (pcase-let ((`(,board-id) arguments))
       (let* ((generation
               (1+ (or (e-board-storage--next-generation storage) 1)))
              (revision (1+ (or (e-board-storage--next-revision storage) 0)))
              (result (list :board-id board-id :generation generation
                            :revision revision)))
         (e-board-storage-sqlite--async-submit
          storage board-id (list :op 'board-clear :board-id board-id) result)
         (setf (e-board-storage--next-generation storage) generation
               (e-board-storage--next-revision storage) revision
               (e-board-storage--next-position storage) 0)
         result)))
    ('publish-record
     (pcase-let ((`(,board-id ,generation ,record ,source) arguments))
       (when source
         (setq source
               (list :kind (plist-get source :kind)
                     :key (plist-get source :key)
                     :hash (e-board-storage-signature-hash
                            (plist-get source :signature)))))
       (let* ((revision (1+ (or (e-board-storage--next-revision storage) 0)))
              (position (1+ (or (e-board-storage--next-position storage) 0)))
              (result (list :board-id board-id :generation generation
                            :revision revision :position position
                            :status 'created :record record)))
         (e-board-storage-sqlite--async-submit
          storage board-id
          (list :op 'board-record-put :board-id board-id :generation generation
                :record record :source source)
          result)
         (setf (e-board-storage--next-revision storage) revision
               (e-board-storage--next-position storage) position)
         result)))
    ('put-participant
     (pcase-let ((`(,board-id ,generation ,participant) arguments))
       (let* ((revision (1+ (or (e-board-storage--next-revision storage) 0)))
              (result (list :board-id board-id :generation generation
                            :revision revision :participant participant)))
         (e-board-storage-sqlite--async-submit
          storage board-id
          (list :op 'board-participant-put :board-id board-id
                :generation generation :participant participant)
          result)
         (setf (e-board-storage--next-revision storage) revision)
         result)))
    ('delete-participant
     (pcase-let ((`(,board-id ,generation ,participant-id) arguments))
       (let* ((revision (1+ (or (e-board-storage--next-revision storage) 0)))
              (result (list :board-id board-id :generation generation
                            :revision revision :participant-id participant-id
                            :deleted t)))
         (e-board-storage-sqlite--async-submit
          storage board-id
          (list :op 'board-participant-delete :board-id board-id
                :generation generation :participant-id participant-id)
          result)
         (setf (e-board-storage--next-revision storage) revision)
         result)))
    ('publish-participant
     (pcase-let ((`(,board-id ,generation ,participant-id) arguments))
       (let* ((revision (1+ (or (e-board-storage--next-revision storage) 0)))
              (result (list :board-id board-id :generation generation
                            :revision revision :participant-id participant-id
                            :publication-pending nil)))
         (e-board-storage-sqlite--async-submit
          storage board-id
          (list :op 'board-participant-publish :board-id board-id
                :generation generation :participant-id participant-id)
          result)
         (setf (e-board-storage--next-revision storage) revision)
         result)))
    ('commit-routing
     (pcase-let ((`(,board-id ,generation ,message-id ,outcome ,pickups)
                   arguments))
       (let* ((revision (1+ (or (e-board-storage--next-revision storage) 0)))
              (committed
               (cl-loop for pickup in pickups
                        for position from 1
                        collect (append pickup
                                        (list :fifo-position position
                                              :revision 1 :state 'ready))))
              (result (list :board-id board-id :generation generation
                            :revision revision :message-id message-id
                            :outcome outcome :pickups committed)))
         (e-board-storage-sqlite--async-submit
          storage board-id
          (list :op 'board-routing-put :board-id board-id
                :generation generation :message-id message-id
                :outcome outcome :pickups (vconcat pickups))
          result)
         (setf (e-board-storage--next-revision storage) revision)
         result)))
    ('transition-pickup
     (pcase-let ((`(,board-id ,generation ,delivery-id ,transition ,data)
                   arguments))
       (let* ((revision (1+ (or (e-board-storage--next-revision storage) 0)))
              (result (list :board-id board-id :generation generation
                            :revision revision :pickup-revision 1)))
         (e-board-storage-sqlite--async-submit
          storage board-id
          (list :op 'board-pickup-transition :board-id board-id
                :generation generation :delivery-id delivery-id
                :transition transition :data data)
          result)
         (setf (e-board-storage--next-revision storage) revision)
         result)))
    ('admit-pickup
     (pcase-let
         ((`(,board-id ,generation ,delivery-id ,session-id ,record ,lane)
           arguments))
       ;; Daily pickup delivery is the directly reached cross-owner write.
       ;; Keep the Board classification timer enqueue-only: the worker owns
       ;; the atomic Board/session transaction and the application-service
       ;; settlement observer owns any later suspect publication.
       (let* ((revision (1+ (or (e-board-storage--next-revision storage) 0)))
              (result (list :board-id board-id :generation generation
                            :board-revision revision :pickup-revision 2
                            :delivery-id delivery-id :session-id session-id
                            :lane lane :record record)))
         (e-board-storage-sqlite--async-submit
          storage board-id
          (list :op 'board-pickup-session-admit :board-id board-id
                :generation generation :delivery-id delivery-id
                :session-id session-id :record record :lane lane)
          result)
         (setf (e-board-storage--next-revision storage) revision)
         result)))
    ('put-replay-progress
     (pcase-let
         ((`(,board-id ,generation ,subscription-id ,position) arguments))
       (let* ((revision (1+ (or (e-board-storage--next-revision storage) 0)))
              (result (list :board-id board-id :generation generation
                            :revision revision :subscription-id subscription-id
                            :position position)))
         (e-board-storage-sqlite--async-submit
          storage board-id
          (list :op 'board-replay-progress-put :board-id board-id
                :generation generation :subscription-id subscription-id
                :position position)
          result)
         (setf (e-board-storage--next-revision storage) revision)
         result)))
    ('status
     (append (list :backend 'sqlite
                   :pending (e-board-storage--pending-count storage)
                   :first-error (copy-tree (e-board-storage--first-error storage)))
             (e-runtime-store-status (e-board-storage--runtime storage))))
    (_
     (signal 'e-board-storage-error
             (list "Interactive SQLite Board operation requires a bounded asynchronous query"
                   operation)))))

(defun e-board-storage-sqlite--call (runtime operation arguments)
  "Dispatch OPERATION ARGUMENTS through RUNTIME."
  (pcase operation
    ('create-board
     (pcase-let ((`(,board-id ,principal ,root) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-create :board-id board-id
              :trusted-principal principal :root root))))
    ('board
     (e-runtime-store-call runtime 'read
                           (list :op 'board-get :board-id (car arguments))))
    ('list-boards
     (pcase-let ((`(,after ,limit) arguments))
       (e-runtime-store-call runtime 'read
                             (list :op 'board-list :after after :limit limit))))
    ('clear-board
     (pcase-let ((`(,board-id) arguments))
       (e-runtime-store-call
        runtime 'write (list :op 'board-clear :board-id board-id))))
    ('publish-record
     (pcase-let ((`(,board-id ,generation ,record ,source) arguments))
       (when source
         (setq source
               (list :kind (plist-get source :kind)
                     :key (plist-get source :key)
                     :hash
                     (e-board-storage-signature-hash
                      (plist-get source :signature)))))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-record-put :board-id board-id :generation generation
              :record record :source source))))
    ('record-page
     (pcase-let ((`(,board-id ,generation ,after ,limit ,selector) arguments))
       (e-runtime-store-call
        runtime 'read
        (list :op 'board-record-page :board-id board-id :generation generation
              :after after :limit limit :selector selector))))
    ('commit-routing
     (pcase-let
         ((`(,board-id ,generation ,message-id ,outcome ,pickups)
           arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-routing-put :board-id board-id :generation generation
              :message-id message-id :outcome outcome
              :pickups (vconcat pickups)))))
    ('routing
     (pcase-let ((`(,board-id ,generation ,message-id) arguments))
       (e-runtime-store-call
        runtime 'read
        (list :op 'board-routing-get :board-id board-id
              :generation generation :message-id message-id))))
    ('transition-pickup
     (pcase-let
         ((`(,board-id ,generation ,delivery-id ,transition ,data)
           arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-pickup-transition :board-id board-id
              :generation generation :delivery-id delivery-id
              :transition transition :data data))))
    ('unresolved-pickups
     (pcase-let ((`(,board-id ,generation ,participant-id ,limit) arguments))
       (e-runtime-store-call
        runtime 'read
        (list :op 'board-pickup-list :board-id board-id :generation generation
              :participant-id participant-id :limit limit))))
    ('put-participant
     (pcase-let ((`(,board-id ,generation ,participant) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-participant-put :board-id board-id
              :generation generation :participant participant))))
    ('delete-participant
     (pcase-let ((`(,board-id ,generation ,participant-id) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-participant-delete :board-id board-id
              :generation generation :participant-id participant-id))))
    ('publish-participant
     (pcase-let ((`(,board-id ,generation ,participant-id) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-participant-publish :board-id board-id
              :generation generation :participant-id participant-id))))
    ('participants
     (pcase-let ((`(,board-id ,generation ,limit) arguments))
       (e-runtime-store-call
        runtime 'read
        (list :op 'board-participant-list :board-id board-id
              :generation generation :limit limit))))
    ('put-replay-progress
     (pcase-let
         ((`(,board-id ,generation ,subscription-id ,position)
           arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-replay-progress-put :board-id board-id
              :generation generation :subscription-id subscription-id
              :position position))))
    ('replay-progress
     (pcase-let ((`(,board-id ,generation ,subscription-id) arguments))
       (e-runtime-store-call
        runtime 'read
        (list :op 'board-replay-progress-get :board-id board-id
              :generation generation :subscription-id subscription-id))))
    ('admit-pickup
     (pcase-let
         ((`(,board-id ,generation ,delivery-id ,session-id ,record ,lane)
           arguments))
       (let ((body
               (list :op 'board-pickup-session-admit :board-id board-id
                     :generation generation :delivery-id delivery-id
                     :session-id session-id
                     :record record :lane lane)))
         (e-runtime-store-call runtime 'write body))))
    ('status
     (append (list :backend 'sqlite)
             (e-runtime-store-status runtime)))
    (_ (signal 'e-board-storage-error
               (list "Unknown Board storage operation" operation)))))

(defun e-board-storage-sqlite-create (runtime)
  "Return a Board storage port backed by shared RUNTIME."
  (unless (e-runtime-store-p runtime)
    (signal 'wrong-type-argument (list 'e-runtime-store-p runtime)))
  (e-board-storage--create
   :runtime runtime
   :call-operation
   (lambda (operation &rest arguments)
     (e-board-storage-sqlite--call runtime operation arguments))))

(defun e-board-storage-sqlite-create-async (runtime)
  "Return the enqueue-return Board storage used by public Daily chat."
  (unless (e-runtime-store-p runtime)
    (signal 'wrong-type-argument (list 'e-runtime-store-p runtime)))
  (let ((storage
         (e-board-storage--create
          :runtime runtime :asynchronous t :pending-count 0
          :next-revision 0 :next-position 0 :next-generation 1)))
    (setf (e-board-storage--call-operation storage)
          (lambda (operation &rest arguments)
            (e-board-storage-sqlite--async-call storage operation arguments)))
    storage))

(provide 'e-board-storage-sqlite)

;;; e-board-storage-sqlite.el ends here
