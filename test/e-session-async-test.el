;;; e-session-async-test.el --- Optimistic session persistence tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-session)
(require 'e-session-async)
(require 'e-session-sqlite)

(defun e-session-async-test--wait (work)
  "Wait up to eight seconds for WORK and return its status."
  (let ((deadline (+ (float-time) 8.0)))
    (while (and (not (e-request-terminal-p (e-work-handle-lifecycle work)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (e-request-terminal-p (e-work-handle-lifecycle work)))
    (e-work-status work)))

(defun e-session-async-test--wait-finished (work)
  "Wait for WORK and return its successful public result."
  (let ((status (e-session-async-test--wait work)))
    (should (eq (plist-get status :state) 'finished))
    (plist-get status :result)))

(defun e-session-async-test--close (store)
  "Close isolated STORE without touching a running Emacs."
  (when store
    (ignore-errors (e-session-sqlite-store-close store))))

(cl-defmacro e-session-async-test--with-held-store
    ((store callbacks operations) &rest body)
  "Run BODY with STORE whose submissions are retained in CALLBACKS/OPERATIONS."
  (declare (indent 1) (debug ((symbolp symbolp symbolp) body)))
  `(let* ((directory (make-temp-file "e-session-optimistic-" t))
          (,store (e-session-sqlite-store-create directory :asynchronous t))
          ,callbacks ,operations)
     (unwind-protect
         (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                    (lambda (_store _session-id body on-settle &optional _escrow)
                      (let ((operation (list :ordinal (1+ (length ,operations))
                                             :body body)))
                        (setq ,operations (append ,operations (list operation))
                              ,callbacks (append ,callbacks (list on-settle)))
                        operation))))
           ,@body)
       (e-session-async-test--close ,store)
       (delete-directory directory t))))

(ert-deftest e-session-async-f92a-public-enable-is-idempotent ()
  "The public enable seam idempotently installs the session service."
  (let* ((directory (make-temp-file "e-session-enable-" t))
         (store (e-session-sqlite-store-create directory)))
    (unwind-protect
        (progn
          (should-not (e-session-async-enabled-p store))
          (should (eq (e-session-enable store) store))
          (should (e-session-async-enabled-p store))
          (should (eq (e-session-enable store) store))
          (should (e-session-async-enabled-p store)))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-accepted-projection-precedes-ack ()
  "Accepted create and dependent messages are readable before acknowledgement."
  (e-session-async-test--with-held-store (store callbacks operations)
    (let ((works
           (list
            (e-session-create store :id "accepted" :metadata '(:name "A"))
            (e-session-append-message
             store "accepted" '(:role user :content "user A"))
            (e-session-append-message
             store "accepted" '(:role assistant :content "assistant A"))
            (e-session-append-message
             store "accepted" '(:role user :content "user B")))))
      (should
       (equal (mapcar (lambda (message) (plist-get message :content))
                      (e-session-messages store "accepted"))
              '("user A" "assistant A" "user B")))
      (should (= (e-session-async-pending-count store "accepted") 4))
      (should (equal (mapcar (lambda (operation)
                              (plist-get (plist-get operation :body) :op))
                            operations)
                     '(session-append session-append session-append
                       session-append)))
      (dolist (callback callbacks) (funcall callback t nil))
      (should (= (e-session-async-pending-count store "accepted") 0))
      (dolist (work works)
        (should (eq (plist-get (e-work-status work) :state) 'finished))))))

(ert-deftest e-session-async-f92a-real-fifo-reopens-equal ()
  "One real runtime FIFO durably reproduces its optimistic projection."
  (let* ((directory (make-temp-file "e-session-real-fifo-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         works expected)
    (unwind-protect
        (progn
          (setq works
                (list
                 (e-session-create store :id "fifo" :metadata '(:name "FIFO"))
                 (e-session-append-message
                  store "fifo" '(:role user :content "first"))
                 (e-session-append-message
                  store "fifo" '(:role assistant :content "second"))))
          (setq expected
                (mapcar (lambda (message)
                          (list (plist-get message :id)
                                (plist-get message :parent-id)
                                (plist-get message :role)
                                (plist-get message :content)))
                        (e-session-messages store "fifo")))
          (dolist (work works) (e-session-async-test--wait-finished work))
          (should-not (e-session-async-pending-p store "fifo"))
          (e-session-async-test--close store)
          (setq store (e-session-sqlite-store-create directory :load-all t))
          (should
           (equal expected
                  (mapcar (lambda (message)
                            (list (plist-get message :id)
                                  (plist-get message :parent-id)
                                  (plist-get message :role)
                                  (plist-get message :content)))
                          (e-session-messages store "fifo")))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-ack-retires-without-republication ()
  "Acknowledgement only retires pending state and never applies a delta twice."
  (e-session-async-test--with-held-store (store callbacks _operations)
    (let* ((create (e-session-create store :id "once"))
           (append (e-session-append-message
                    store "once" '(:role user :content "once")))
           (sequence (e-session-store-sequence store)))
      (should (= (length (e-session-messages store "once")) 1))
      (dolist (callback callbacks) (funcall callback t nil))
      (should (= (length (e-session-messages store "once")) 1))
      (should (= (e-session-store-sequence store) sequence))
      (should (eq (plist-get (e-work-status create) :state) 'finished))
      (should (eq (plist-get (e-work-status append) :state) 'finished)))))

(ert-deftest e-session-async-f92a-caller-input-is-detached-before-admission ()
  "Caller mutation after return cannot alter the effective aggregate."
  (e-session-async-test--with-held-store (store _callbacks _operations)
    (let* ((content (copy-sequence "original"))
           (message (list :role 'user :content content)))
      (e-session-create store :id "detached")
      (e-session-append-message store "detached" message)
      (aset content 0 ?X)
      (plist-put message :content "replacement")
      (should (equal (plist-get (car (e-session-messages store "detached"))
                                :content)
                     "original")))))

(ert-deftest e-session-async-f92a-noop-submits-and-dirties-nothing ()
  "A semantic no-op produces no runtime request or projection obligation."
  (let* ((directory (make-temp-file "e-session-noop-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t)))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "noop"))
          (let* ((state (e-session-storage--state store))
                 (dirty (hash-table-count
                         (e-session-storage--state-checkpoint-dirty-session-ids
                          state))))
            (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                       (lambda (&rest _)
                         (ert-fail "No-op reached physical submission"))))
              (let ((work (e-session-set-message-display
                           store "noop" "missing" 'hidden)))
                (should (eq (plist-get (e-work-status work) :state) 'finished))
                (should-not (plist-get (e-work-status work) :result))))
            (should (= dirty
                       (hash-table-count
                        (e-session-storage--state-checkpoint-dirty-session-ids
                         state))))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-suspect-owner-allows-only-semantic-noops ()
  "A suspect owner permits three no-ops but rejects physical writes."
  (let* ((directory (make-temp-file "e-session-suspect-noops-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (real-submit (symbol-function 'e-session-storage-submit-owned))
         (package
          '(:promotion
            (:record-version 3 :type context-promotion
             :id "suspect-curation" :frame-id "suspect-frame"
             :generation-id "suspect-generation"
             :consumer-request-id "suspect-consumer"
             :response-entry-id "suspect-response"
             :items ((:kind exact :value "kept"
                      :source-observation-ids ("suspect-observation")
                      :source-refs ("suspect-source")
                      :source-fingerprints ("suspect-fingerprint"))))
            :erasure nil))
         (envelope '(:id "suspect-board-message" :kind output
                     :content "retained"))
         retained-board (submissions 0))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "suspect-noops"))
          (e-session-async-test--wait-finished
           (e-session-append-message
            store "suspect-noops"
            '(:id "present-message" :role user :content "present")))
          (e-session-async-test--wait-finished
           (e-session-append-context-generation
            store "suspect-noops"
            (e-context-lifetime-generation-create
             :id "suspect-generation" :checkpoint nil
             :covered-session-boundary
             (plist-get (e-session-get store "suspect-noops")
                        :root-event-id))))
          (e-session-async-test--wait-finished
           (e-session-append-context-curation-package
            store "suspect-noops" package))
          (setq retained-board
                (e-session-async-test--wait-finished
                 (e-session-append-board-message
                  store "suspect-noops" envelope)))
          (e-session-note-persistence-failure
           store "suspect-noops"
           '(e-session-storage-error "owner is suspect"))
          (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                     (lambda (&rest arguments)
                       (cl-incf submissions)
                       (apply real-submit arguments))))
            (let ((before submissions))
              (should-not
               (e-session-async-test--wait-finished
                (e-session-set-message-display
                 store "suspect-noops" "missing-message" 'hidden)))
              (should (= submissions before)))
            (let ((before submissions)
                  (duplicate
                   (e-session-async-test--wait-finished
                    (e-session-append-context-curation-package
                     store "suspect-noops" package))))
              (should (plist-get duplicate :already-present))
              (should-not (plist-member duplicate :record))
              (should (= submissions before)))
            (let ((before submissions)
                  (duplicate
                   (e-session-async-test--wait-finished
                    (e-session-append-board-message
                     store "suspect-noops" retained-board))))
              (should (equal duplicate retained-board))
              (should-not (eq duplicate retained-board))
              (should (= submissions before)))
            (let ((before submissions)
                  (rejected
                   (e-session-append-message
                    store "suspect-noops"
                    '(:role user :content "must be fenced"))))
              (should (eq (plist-get (e-work-status rejected) :state) 'failed))
              (should (= submissions before)))
            (should
             (equal
              (plist-get
               (e-session-async-test--wait-finished
                (e-session-create store :id "suspect-sibling"))
               :id)
              "suspect-sibling"))
            (should (= submissions 1))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-observer-install-drop-publishes-nothing ()
  "An observer-install fault drops its exact queued request before projection."
  (let* ((directory (make-temp-file "e-session-observer-drop-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'e-runtime-store--observe)
                     (lambda (&rest _)
                       (error "observer install fault"))))
            (let ((work (e-session-create store :id "observer-drop")))
              (should (eq (plist-get (e-work-status work) :state) 'failed))))
          (should-error (e-session-get store "observer-drop")
                        :type 'e-session-missing)
          (should (= (e-session-async-pending-count store "observer-drop") 0))
          (should-not
           (e-session-async-session-suspect store "observer-drop"))
          (e-session-async-test--close store)
          (setq store (e-session-sqlite-store-create directory :load-all t))
          (should-error (e-session-get store "observer-drop")
                        :type 'e-session-missing))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-observer-install-ambiguous-retains-owner ()
  "Ambiguous observer installation retains pending/suspect until settlement."
  (let* ((directory (make-temp-file "e-session-observer-ambiguous-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         request cancelled)
    (unwind-protect
        (cl-letf (((symbol-function 'e-runtime-store--submit-owned)
                   (lambda (_runtime kind body owner-key &optional _escrow)
                     (setq request
                           (e-runtime-store-request--create
                            :id "observer-ambiguous-request"
                            :kind kind :body body :owner-key owner-key
                            :state 'submitted))))
                  ((symbol-function 'e-runtime-store--observe)
                   (lambda (&rest _)
                     (error "observer install fault")))
                  ((symbol-function 'e-runtime-store-cancel)
                   (lambda (_runtime candidate)
                     (setq cancelled candidate)
                     'in-flight)))
          (let ((work (e-session-create store :id "observer-ambiguous")))
            (should (eq cancelled request))
            (should (eq (plist-get (e-work-status work) :state) 'failed))
            (should (= (e-session-async-pending-count
                        store "observer-ambiguous")
                       1))
            (should
             (e-session-async-session-suspect store "observer-ambiguous"))
            (should-not
             (plist-get
              (cddr (e-session-async-session-suspect
                     store "observer-ambiguous"))
              :operation))
            (should-not
             (gethash "observer-ambiguous" (e-session-store-sessions store)))
            (should (functionp
                     (e-runtime-store-request--observer request)))
            (setf (e-runtime-store-request--state request) 'failed
                  (e-runtime-store-request--error request)
                  '(e-session-storage-error "terminal failure"))
            (funcall (e-runtime-store-request--observer request) request)
            (should (= (e-session-async-pending-count
                        store "observer-ambiguous")
                       0))
            (should
             (e-session-async-session-suspect store "observer-ambiguous"))
            (should-not
             (gethash "observer-ambiguous"
                      (e-session-store-sessions store)))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-before-install-drops-exact-request ()
  "A before-install fault drops the queued request and publishes nothing."
  (let* ((directory (make-temp-file "e-session-before-install-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         callback operation cancelled)
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                   (lambda (_store _session-id _body on-settle &optional _escrow)
                     (setq callback on-settle operation (list :request "exact"))
                     operation))
                  ((symbol-function 'e-session-storage-cancel-operation)
                   (lambda (_store candidate)
                     (setq cancelled candidate)
                     'dropped))
                  (e-session-async--install-fault-function
                   (lambda (edge)
                     (when (eq edge 'before-install) (error "before install")))))
          (let ((work (e-session-create store :id "before-install")))
            (should (eq cancelled operation))
            (should (eq (plist-get (e-work-status work) :state) 'failed))
            (should-error (e-session-get store "before-install")
                          :type 'e-session-missing)
            (should (= (e-session-async-pending-count store "before-install") 0))
            (should-not (e-session-async-session-suspect
                         store "before-install")))
          (should callback))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-pending-registration-precedes-submit ()
  "Pending registration failure has no request, projection, or ownership."
  (let* ((directory (make-temp-file "e-session-pending-registration-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         submitted)
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                   (lambda (&rest _arguments) (setq submitted t)))
                  (e-session-async--install-fault-function
                   (lambda (edge)
                     (when (eq edge 'pending-registration)
                       (error "pending registration")))))
          (let ((work (e-session-create store :id "pending-registration")))
            (should (eq (plist-get (e-work-status work) :state) 'failed))
            (should-not submitted)
            (should-error (e-session-get store "pending-registration")
                          :type 'e-session-missing)
            (should (= (e-session-async-pending-count
                        store "pending-registration")
                       0))
            (should-not (e-session-async-session-suspect
                         store "pending-registration"))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-apply-fault-rolls-back-before-cancel ()
  "A transactional install failure restores the aggregate before exact cancel."
  (let* ((directory (make-temp-file "e-session-apply-fault-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t)))
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                   (lambda (&rest _) 'accepted-operation))
                  ((symbol-function 'e-session-storage-cancel-operation)
                   (lambda (&rest _) 'dropped))
                  (e-session-aggregate--committed-apply-fault-function
                   (lambda (edge)
                     (when (eq edge 'before-record) (error "apply fault")))))
          (let ((work (e-session-create store :id "apply-fault")))
            (should (eq (plist-get (e-work-status work) :state) 'failed))
            (should-error (e-session-get store "apply-fault")
                          :type 'e-session-missing)
            (should-not (e-session-async-session-suspect store "apply-fault"))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-after-install-keeps-whole-projection-suspect ()
  "An after-install fault keeps the complete projection and marks it suspect."
  (let* ((directory (make-temp-file "e-session-after-install-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         callback cancelled)
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                   (lambda (_store _session-id _body on-settle &optional _escrow)
                     (setq callback on-settle)
                     'accepted-operation))
                  ((symbol-function 'e-session-storage-cancel-operation)
                   (lambda (&rest _) (setq cancelled t) 'dropped))
                  (e-session-async--install-fault-function
                   (lambda (edge)
                     (when (eq edge 'after-install) (error "after install")))))
          (let ((work (e-session-create store :id "after-install")))
            (should (equal (plist-get (e-session-get store "after-install") :id)
                           "after-install"))
            (should-not cancelled)
            (should (eq (plist-get (e-work-status work) :state) 'failed))
            (should (e-session-async-session-suspect store "after-install"))
            (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                       (lambda (&rest _)
                         (ert-fail "Suspect owner reached submission"))))
              (let ((later (e-session-append-message
                            store "after-install"
                            '(:role user :content "blocked"))))
                (should (eq (plist-get (e-work-status later) :state) 'failed))))
            (funcall callback t nil)
            (should (eq (plist-get (e-work-status work) :state) 'failed))
            (should (= (e-session-async-pending-count store "after-install") 0))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-ambiguous-cancel-fences-only-owner ()
  "An ambiguous pre-install cancellation fences its owner, not a sibling."
  (e-session-async-test--with-held-store (store callbacks operations)
    (let ((fault-once t)
          cancelled)
      (cl-letf (((symbol-function 'e-session-storage-cancel-operation)
                 (lambda (_store operation)
                   (setq cancelled operation)
                   'submitted))
                (e-session-async--install-fault-function
                 (lambda (edge)
                   (when (and fault-once (eq edge 'before-install))
                     (setq fault-once nil)
                     (error "ambiguous install")))))
        (let ((first (e-session-create store :id "ambiguous")))
          (should (eq (plist-get (e-work-status first) :state) 'failed))
          (should cancelled)
          (should (e-session-async-session-suspect store "ambiguous"))
          (let ((before (length operations))
                (rejected (e-session-create store :id "ambiguous")))
            (should (= (length operations) before))
            (should (eq (plist-get (e-work-status rejected) :state) 'failed)))
          (let ((sibling (e-session-create store :id "healthy")))
            (should (equal (plist-get (e-session-get store "healthy") :id)
                           "healthy"))
            (funcall (cadr callbacks) t nil)
            (should (eq (plist-get (e-work-status sibling) :state) 'finished)))
          (funcall (car callbacks) nil
                   '(e-session-storage-error "ambiguous request failed"))
          (should (= (e-session-async-pending-count store "ambiguous") 0)))))))

(ert-deftest e-session-async-f92a-terminal-state-precedes-work-callbacks ()
  "Pending and suspect state settle before callbacks that error or quit."
  (dolist (exit '(error quit))
    (e-session-async-test--with-held-store (store callbacks _operations)
      (let ((seen nil)
            (work (e-session-create store :id (symbol-name exit))))
        (e-work-on-settle
         work
         (lambda (_settled)
           (setq seen
                 (list (e-session-async-pending-count store (symbol-name exit))
                       (and (e-session-async-session-suspect
                             store (symbol-name exit)) t)))
           (signal exit (list "observer exit"))))
        (condition-case nil
            (funcall (car callbacks) nil
                     '(e-runtime-store-timeout "write failed"))
          (error nil)
          (quit nil))
        (should (equal seen '(0 t)))
        (should (eq (plist-get (e-work-status work) :state) 'failed))))))

(ert-deftest e-session-async-f92a-close-isolates-pending-work-observers ()
  "Close detaches all pending work despite error/quit observers, then closes runtime."
  (e-session-async-test--with-held-store (store _callbacks _operations)
    (let* ((first (e-session-create store :id "close-first"))
           (second (e-session-create store :id "close-second"))
           (state (gethash store e-session-async--states))
           (internal
            (append (gethash "close-first" (e-session-async--state-pending state))
                    (gethash "close-second" (e-session-async--state-pending state))))
           closed
           observed)
      (e-work-on-settle first
                        (lambda (_work) (push 'error observed)
                          (error "first observer")))
      (e-work-on-settle second
                        (lambda (_work) (push 'quit observed)
                          (signal 'quit '("second observer"))))
      (cl-letf (((symbol-function 'e-session-storage-sqlite-close)
                 (lambda (_store) (setq closed t))))
        (e-session-storage-close store))
      (should closed)
      (should (equal (sort observed
                           (lambda (left right)
                             (string< (symbol-name left) (symbol-name right))))
                     '(error quit)))
      (should (eq (plist-get (e-work-status first) :state) 'failed))
      (should (eq (plist-get (e-work-status second) :state) 'failed))
      (should-not (gethash store e-session-async--states))
      (dolist (operation internal)
        (should (e-session-async--operation-settled operation))
        (should-not (e-session-async--operation-state operation))
        (should-not (e-session-async--operation-token operation))
        (should-not (e-session-async--operation-storage-operation operation))))))

(ert-deftest e-session-async-f92a-first-suspect-is-bounded-and-reset-clears ()
  "Repeated failures retain one bounded first cause until reset or close."
  (e-session-async-test--with-held-store (store callbacks _operations)
    (let ((first (e-session-create store :id "suspect")))
      (funcall (car callbacks) nil
               (list 'e-runtime-store-timeout (make-string 4096 ?x)
                     :request-id "request-1" :operation 'session-append))
      (let ((cause (e-session-async-session-suspect store "suspect")))
        (should (<= (string-bytes (cadr cause)) 1024))
        (e-session-async--note-suspect
         store "suspect" '(e-session-storage-error "second"))
        (should (equal (e-session-async-session-suspect store "suspect") cause)))
      (should (eq (plist-get (e-work-status first) :state) 'failed))
      (e-session-async-reset store)
      (should-not (e-session-async-session-suspect store "suspect"))
      (should (= (e-session-async-pending-count store "suspect") 0)))))

(ert-deftest e-session-async-f92a-failed-owner-does-not-block-sibling-readback ()
  "One failed owner remains suspect while a sibling persists and reopens."
  (let* ((directory (make-temp-file "e-session-sibling-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (real-submit (symbol-function 'e-session-storage-submit-owned))
         failed-callback sibling-work)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                     (lambda (current session-id body on-settle &optional escrow)
                       (if (equal session-id "failed")
                           (progn (setq failed-callback on-settle)
                                  'failed-operation)
                         (funcall real-submit current session-id body on-settle
                                  escrow)))))
            (e-session-create store :id "failed")
            (setq sibling-work
                  (e-session-create store :id "sibling"
                                    :metadata '(:name "Sibling"))))
          (funcall failed-callback nil
                   '(e-runtime-store-timeout "failed owner"))
          (e-session-async-test--wait-finished sibling-work)
          (should (e-session-async-session-suspect store "failed"))
          (should-not (e-session-async-session-suspect store "sibling"))
          (e-session-async-test--close store)
          (setq store (e-session-sqlite-store-create directory :load-all t))
          (should (equal (plist-get (e-session-get store "sibling") :id)
                         "sibling")))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-practical-record-limit-rejects-before-enqueue ()
  "The ordinary record cap rejects an oversized write before FIFO admission."
  (let* ((directory (make-temp-file "e-session-record-cap-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         touched)
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "capacity"))
          (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                     (lambda (&rest _) (setq touched t))))
            (let* ((work (e-session-append-message
                          store "capacity"
                          (list :role 'user
                                :content (make-string 1100000 ?x))))
                   (status (e-work-status work)))
              (should (eq (plist-get status :state) 'failed))
              (should (eq (car (plist-get status :error))
                          'e-session-async-capacity-exhausted))
              (should-not touched))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-practical-preflight-precedes-freeze ()
  "Exact practical input passes; one-over, gross, and cyclic inputs never freeze."
  (let* ((empty (list :message (list :role 'user :content "")))
         (base (plist-get
                (e-session-aggregate-command-practical-preflight empty)
                :bytes))
         (remaining (- e-session-aggregate-command-practical-byte-limit base))
         (exact (list :message
                      (list :role 'user :content (make-string remaining ?x))))
         (one-over (list :message
                         (list :role 'user
                               :content (make-string (1+ remaining) ?x))))
         (gross (list :message
                      (list :role 'user :content (make-string (* 2 1024 1024) ?x))))
         (cycle (list :role 'user)))
    (setcdr (last cycle) cycle)
    (should (= (plist-get
                (e-session-aggregate-command-practical-preflight exact)
                :bytes)
               e-session-aggregate-command-practical-byte-limit))
    (dolist (arguments (list one-over gross (list :message cycle)))
      (let (froze)
        (cl-letf (((symbol-function 'e-session-aggregate-command-freeze)
                   (lambda (&rest _) (setq froze t)
                     (ert-fail "Rejected producer reached freeze"))))
          (should-error
           (e-session-aggregate-command-prepare
            'append-message "preflight" arguments)))
        (should-not froze)))))

(ert-deftest e-session-async-f92a-fork-remains-typed-unsupported ()
  "Asynchronous fork remains an explicit terminal unsupported command."
  (let* ((directory (make-temp-file "e-session-fork-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t)))
    (unwind-protect
        (let* ((work (e-session-fork store "source"))
               (status (e-work-status work)))
          (should (eq (plist-get status :state) 'failed))
          (should (eq (car (plist-get status :error))
                      'e-session-storage-command-error)))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-public-facade-families-reopen-equal ()
  "Every async facade family returns its legacy shape and survives reopen."
  (let* ((directory (make-temp-file "e-session-facades-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (session-id "facades"))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id session-id
                             :metadata '(:project-root "/initial/")))
          (should
           (eq (plist-get
                (e-session-async-test--wait-finished
                 (e-session-append-activity-event
                  store session-id "turn" 'note '(:value 1)))
                :event-type)
               'note))
          (should
           (equal (plist-get
                   (e-session-async-test--wait-finished
                    (e-session-append-context-curation-response
                     store session-id "turn" "response-entry"))
                   :id)
                  "response-entry"))
          (let ((message
                 (e-session-async-test--wait-finished
                  (e-session-append-message
                   store session-id
                   '(:id "display-me" :role user :content "visible")))))
            (should
             (eq (plist-get
                  (e-session-async-test--wait-finished
                   (e-session-set-message-display
                    store session-id (plist-get message :id) 'hidden))
                  :display)
                 'hidden)))
          (should
           (equal (plist-get
                   (e-session-async-test--wait-finished
                    (e-session-append-process-report
                     store session-id '(:kind note :value "report")))
                   :value)
                  "report"))
          (should
           (equal (plist-get
                   (e-session-async-test--wait-finished
                    (e-session-append-branch-summary
                     store session-id "branch-summary" "summary"))
                   :summary)
                  "summary"))
          (should
           (equal (plist-get
                   (e-session-async-test--wait-finished
                    (e-session-append-compaction
                     store session-id "compact" :tokens-before 10
                     :tokens-kept 3))
                   :summary)
                  "compact"))
          (should
           (eq (plist-get
                (e-session-async-test--wait-finished
                 (e-session-append-provider-anchor
                  store session-id 'openai :model "model-a"
                  :covered-entry-id "display-me"))
                :provider-id)
               'openai))
          (let* ((root-id (plist-get (e-session-get store session-id)
                                     :root-event-id))
                 (generation
                  (e-context-lifetime-generation-create
                   :id "generation-async" :checkpoint nil
                   :covered-session-boundary root-id)))
            (should
             (eq (plist-get
                  (e-session-async-test--wait-finished
                   (e-session-append-context-generation
                    store session-id generation))
                  :type)
                 'context-generation)))
          (let* ((promotion
                  '(:record-version 3 :type context-promotion
                    :id "curation-async" :frame-id "frame-async"
                    :generation-id "generation-async"
                    :consumer-request-id "consumer-async"
                    :response-entry-id "response-async"
                    :items ((:kind exact :value "kept"
                             :source-observation-ids ("observation-async")
                             :source-refs ("source-async")
                             :source-fingerprints ("fingerprint-async")))))
                 (package (list :promotion promotion :erasure nil))
                 (first
                  (e-session-async-test--wait-finished
                   (e-session-append-context-curation-package
                    store session-id package)))
                 duplicate)
            (should (plist-member first :record))
            (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                       (lambda (&rest _)
                         (ert-fail "duplicate curation package submitted"))))
              (setq duplicate
                    (e-session-async-test--wait-finished
                     (e-session-append-context-curation-package
                      store session-id package))))
            (should (plist-get duplicate :already-present))
            (should-not (plist-member duplicate :record)))
          (let* ((envelope '(:id "board-message" :kind output
                             :content "detached"))
                 (first
                  (e-session-async-test--wait-finished
                   (e-session-append-board-message
                    store session-id envelope)))
                 duplicate)
            (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                       (lambda (&rest _)
                         (ert-fail "duplicate Board envelope submitted"))))
              (setq duplicate
                    (e-session-async-test--wait-finished
                     (e-session-append-board-message
                      store session-id first))))
            (should (equal duplicate first))
            (should-not (eq duplicate first)))
          (should
           (equal
            (e-session-async-test--wait-finished
             (e-session-declare-board-state
              store session-id "principal" "board" "participant" nil))
            '(:board-id "board" :principal "principal"
              :association-role "participant")))
          (should-not
           (e-session-async-test--wait-finished
            (e-session-clear-board-messages store session-id)))
          (should
           (equal (e-session-async-test--wait-finished
                   (e-session-set-metadata
                    store session-id '(:project-root "/meta/")))
                  '(:project-root "/meta/")))
          (should
           (equal (e-session-async-test--wait-finished
                   (e-session-set-session-config
                    store session-id '(:model "model-a")))
                  '(:project-root "/meta/" :model "model-a")))
          (let ((references
                 '(:attachments ((:uri "buffer://source" :id "source")))))
            (should
             (equal (e-session-async-test--wait-finished
                     (e-session-set-context-references
                      store session-id 'chat-session references))
                    references)))
          (should
           (plist-get
            (e-session-async-test--wait-finished
             (e-session-set-context-reference
              store session-id :org-canvas-ref '(:uri "buffer://canvas")))
            :org-canvas-ref))
          (should
           (equal (e-session-async-test--wait-finished
                   (e-session-set-capability-state
                    store session-id 'mcp '(:enabled t) :version 2))
                  '(:version 2 :state (:enabled t))))
          (should
           (equal (e-session-async-test--wait-finished
                   (e-session-set-turn-options
                    store session-id '(:model "model-b")))
                  '(:model "model-b")))
          (should
           (equal (e-session-async-test--wait-finished
                   (e-session-set-current-branch
                    store session-id "branch-a"))
                  "branch-a"))
          (should
           (eq (e-session-async-test--wait-finished
                (e-session-rename store session-id "Facade session"))
               (e-session-get store session-id)))
          (e-session-async-test--wait-finished
           (e-session-create store :id "facades-clear"))
          (e-session-async-test--wait-finished
           (e-session-append-message
            store "facades-clear" '(:role user :content "clear me")))
          (should
           (eq (plist-get
                (e-session-async-test--wait-finished
                 (e-session-clear-messages store "facades-clear"))
                :event-type)
               'messages-cleared))
          (e-session-async-test--close store)
          (setq store (e-session-sqlite-store-create directory :load-all t))
          (let ((session (e-session-get store session-id)))
            (should-not (e-session-messages store "facades-clear"))
            (should (= (length (e-session-messages store session-id)) 1))
            (should (equal (plist-get (plist-get session :metadata) :model)
                           "model-a"))
            (should (equal (plist-get session :turn-options)
                           '(:model "model-b")))
            (should (equal (plist-get session :current-branch) "branch-a"))
            (should (equal (plist-get session :name) "Facade session"))
            (should (= (length (plist-get session :activity-events)) 2))
            (should (= (length (plist-get session :branch-summaries)) 1))
            (should (= (length (plist-get session :compactions)) 1))
            (should (= (length (plist-get session :provider-anchors)) 1))
            (should (= (length (plist-get session :process-reports)) 1))
            (should (= (length (e-session-context-generations
                                store session-id))
                       1))
            (should (= (length (e-session-context-promotions
                                store session-id))
                       1))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-delete-result-and-reopen-absence ()
  "Async delete returns t and reopens absent."
  (let* ((directory (make-temp-file "e-session-delete-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t)))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "deleted"))
          (should
           (eq (e-session-async-test--wait-finished
                (e-session-delete store "deleted"))
               t))
          (should-not (e-session-session-present-p store "deleted"))
          (e-session-async-test--close store)
          (setq store (e-session-sqlite-store-create directory :load-all t))
          (should-not (e-session-session-present-p store "deleted")))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-f92a-storage-port-has-no-session-suspect-policy ()
  "The physical storage port exposes no session-domain suspect registry."
  (should-not (fboundp 'e-session-storage-session-suspect))
  (should-not (fboundp 'e-session-storage--note-session-suspect)))

(provide 'e-session-async-test)

;;; e-session-async-test.el ends here
