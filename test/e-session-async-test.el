;;; e-session-async-test.el --- Session sealed-command coordinator tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-session)
(require 'e-session-async)
(require 'e-session-sqlite)

(defun e-session-async-test--wait (work)
  "Run cooperative progress until WORK settles and return its status."
  (let ((deadline (+ (float-time) 8.0)))
    (while (and (not (e-request-terminal-p (e-work-handle-lifecycle work)))
                (< (float-time) deadline))
      (sit-for 0.01))
    (should (e-request-terminal-p (e-work-handle-lifecycle work)))
    (e-work-status work)))

(defun e-session-async-test--wait-finished (work)
  "Wait for WORK and return its finished result."
  (let ((status (e-session-async-test--wait work)))
    (should (eq (plist-get status :state) 'finished))
    (plist-get status :result)))

(defun e-session-async-test--close (store)
  "Retire settled STORE without touching any external Emacs."
  (when (e-session-async-enabled-p store)
    (e-session-async-teardown store))
  (ignore-errors (e-session-sqlite-store-close store)))

(defun e-session-async-test--assert-zero-ownership (store)
  "Assert STORE has no session or shared composition ownership."
  (let* ((coordinator (e-session-async-coordinator store))
         (runtime (e-session-storage-runtime-store store)))
    (should (= (e-session-async--coordinator-count coordinator) 0))
    (should (= (e-session-async--coordinator-bytes coordinator) 0))
    (should (= (hash-table-count
                (e-session-async--coordinator-lanes coordinator)) 0))
    (should (= (e-runtime-store--reservation-used
                (e-runtime-store--reservation runtime)) 0))))

(defun e-session-async-test--seal (store tag session-id arguments)
  "Seal one command with STORE's production frame-accounting adapter."
  (let* ((measure (if (e-session-storage-sqlite-p store)
                      (lambda (body)
                        (e-session-storage-measure-frame-escrow
                         store 'write body))
                    (lambda (body)
                      (e-runtime-store-codec-measure-bounded
                       body e-runtime-store-codec-protocol-canonical-byte-limit))))
         (accounting (e-session-aggregate-command-accounting
                      tag session-id arguments measure)))
    (e-session-aggregate-command-seal
     tag session-id arguments accounting measure)))

(defun e-session-async-test--balanced-cons-tree (nodes)
  "Return an acyclic balanced tree containing exactly NODES cons cells."
  (when (> nodes 0)
    (let* ((remaining (1- nodes))
           (left (/ remaining 2)))
      (cons (e-session-async-test--balanced-cons-tree left)
            (e-session-async-test--balanced-cons-tree (- remaining left))))))

(defun e-session-async-test--node-limit-arguments (tag node-count)
  "Return TAG arguments containing exactly NODE-COUNT measured nodes.

Each graph contains nested cons, vector, and hash containers plus one separately
charged hash entry."
  (let* ((wrapper-nodes (pcase tag
                          ('create 4)
                          ('append-message 6)
                          ('session-info 6)
                          (_ 4)))
         ;; Hash + one entry + vector account for three further nodes.
         (tree-nodes (- node-count wrapper-nodes 3))
         (table (make-hash-table :test 'eq)))
    (when (< tree-nodes 0)
      (error "Node fixture is smaller than its command wrapper"))
    (puthash :nested
             (vector (e-session-async-test--balanced-cons-tree tree-nodes))
             table)
    (pcase tag
      ('create (list :metadata (list :project-root table)))
      ('append-message (list :message (list :role 'user :content table)))
      ('session-info
       (list :field 'metadata :value (list :project-root table))))))

(ert-deftest e-session-async-s92-domain-accounting-schema-is-exact ()
  "One aggregate schema owns generated D/R and exact family frame escrow."
  (let* ((directory (make-temp-file "e-session-accounting-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (cases '((create (:metadata nil) 262144 16777216)
                  (append-message (:message (:role user :content "x"))
                                  131072 16777216)
                  (session-info (:field name :value "name") 98304 16777216)
                  (message-display (:message-id "m" :display hidden)
                                   32768 65536))))
    (unwind-protect
        (dolist (case cases)
          (pcase-let ((`(,tag ,arguments ,expected-d ,expected-p) case))
            (let* ((measure
                    (lambda (body)
                      (e-session-storage-measure-frame-escrow
                       store 'write body)))
                   (accounting
                    (e-session-aggregate-command-accounting
                     tag "accounting" arguments measure))
                   (maximum-body
                    (e-session-aggregate-command-maximum-transport-body
                     tag arguments)))
              (should (= (plist-get accounting :delta-bytes) expected-d))
              (should (= (plist-get accounting :producer-max) expected-p))
              (should (= (plist-get accounting :reference-bytes) 8192))
              (should (= (plist-get accounting :local-bytes)
                         (+ (plist-get accounting :producer-bytes)
                            expected-d 8192)))
              (should (= (plist-get accounting :frame-reserve)
                         (funcall measure maximum-body))))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-producer-max-and-one-over-are-exact ()
  "Unique producer leaves admit exactly 16 MiB and reject its first byte over."
  (let* ((limit 16777216)
         (exact (make-string limit ?x))
         (one-over (make-string (1+ limit) ?x)))
    (should (= (e-session-aggregate-command-measure-producer exact limit)
               limit))
    (should-error
     (e-session-aggregate-command-measure-producer one-over limit)
     :type 'e-session-command-too-large)
    (should (= (+ limit 262144 8192)
               e-session-async-lane-byte-capacity))))

(ert-deftest e-session-async-s92-producer-node-limits-are-exact-by-family ()
  "Nested producer containers consume generated D slots before construction."
  (let* ((directory (make-temp-file "e-session-node-accounting-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (cases '((create create 1024 224 262144)
                  (append append-message 512 256 131072)
                  (state session-info 384 256 98304))))
    (unwind-protect
        (dolist (case cases)
          (pcase-let ((`(,family ,tag ,node-limit ,fixed-nodes ,expected-d) case))
            (let* ((exact (e-session-async-test--node-limit-arguments
                           tag (- node-limit fixed-nodes)))
                   (one-over (e-session-async-test--node-limit-arguments
                              tag (1+ (- node-limit fixed-nodes))))
                   (accounting
                    (e-session-aggregate-command-accounting
                     tag (format "nodes-%s" family) exact (lambda (_body) 1)))
                   frame-called seal-called freeze-called identity-called)
              (should (= (plist-get accounting :producer-nodes)
                         (- node-limit fixed-nodes)))
              (should (= (plist-get accounting :producer-node-limit)
                         (- node-limit fixed-nodes)))
              (should (= (plist-get accounting :fixed-nodes) fixed-nodes))
              (should (= (plist-get accounting :total-nodes) node-limit))
              (should (= (plist-get accounting :accounted-container-bytes)
                         expected-d))
              (should (= (+ (plist-get accounting :producer-container-bytes)
                            (plist-get accounting :fixed-container-bytes))
                         expected-d))
              (should (= (plist-get accounting :delta-bytes) expected-d))
              (cl-letf (((symbol-function 'e-session-storage-measure-frame-escrow)
                         (lambda (&rest _)
                           (setq frame-called t)
                           (ert-fail "one-over graph reached frame measurement")))
                        ((symbol-function 'e-session-aggregate-command-freeze)
                         (lambda (&rest _)
                           (setq freeze-called t)
                           (ert-fail "one-over graph reached freeze")))
                        ((symbol-function 'e-session-aggregate-command-seal)
                         (lambda (&rest _)
                           (setq seal-called t)
                           (ert-fail "one-over graph reached construction")))
                        ((symbol-function 'e-session-identity-generate-ulid)
                         (lambda (&rest _)
                           (setq identity-called t)
                           (ert-fail "one-over graph reached identity allocation"))))
                (let ((work (e-session-async-submit-command
                             store (format "nodes-over-%s" family) tag one-over)))
                  (should (eq (plist-get (e-work-status work) :state) 'failed))))
              (should-not frame-called)
              (should-not seal-called)
              (should-not freeze-called)
              (should-not identity-called))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-producer-node-sharing-and-cycles-are-exact ()
  "Shared containers charge once while a back-edge is rejected deterministically."
  (let* ((leaf (copy-sequence "one-leaf"))
         (shared (cons leaf nil))
         (table (make-hash-table :test 'eq))
         (vector (vector shared table shared))
         (arguments (list :value vector :same vector)))
    (puthash :first shared table)
    (puthash :second shared table)
    (let ((measurement
           (e-session-aggregate--command-measure-producer-graph
            arguments 1024 1024)))
      ;; Four argument-spine conses, one vector, one shared cons, one hash,
      ;; and two hash-entry nodes.  The shared vector/cons/string charge once.
      (should (= (plist-get measurement :nodes) 9))
      (should (= (plist-get measurement :bytes) (string-bytes leaf))))
    (let* ((frozen (e-session-aggregate-command-freeze arguments))
           (frozen-vector (plist-get frozen :value))
           (frozen-shared (aref frozen-vector 0)))
      (should (eq frozen-vector (plist-get frozen :same)))
      (should (eq frozen-shared (aref frozen-vector 2)))
      (should-not (eq frozen-shared shared)))
    (let ((cycle (cons nil nil)))
      (setcar cycle cycle)
      (should-error
       (e-session-aggregate-command-measure-producer cycle 1024 1024)
       :type 'e-session-error))))

(ert-deftest e-session-async-s92-actual-maximum-frame-fits-family-reserve ()
  "Each family's maximum producer yields an actual frame within Freserve."
  (let* ((store (e-session-store-create))
         (_session (e-session-aggregate-create store :id "frame-base"))
         (maximum (make-string 16777216 ?x))
         (cases
          `((create "frame-create" (:metadata (:name ,maximum)))
            (append-message "frame-base"
                            (:message (:role user :created-at ,maximum)))
            (session-info "frame-base"
                          (:field metadata :value (:project-root ,maximum)))))
         (measure
          (lambda (body)
            (e-runtime-store-codec-measure-bounded
             body e-runtime-store-codec-protocol-canonical-byte-limit))))
    (dolist (case cases)
      (pcase-let ((`(,tag ,session-id ,arguments) case))
        (let* ((accounting (e-session-aggregate-command-accounting
                            tag session-id arguments measure))
               (command (e-session-aggregate-command-seal
                         tag session-id arguments accounting measure))
               (delta (e-session-aggregate-command-interpret store command))
               (actual-body
                (list :op 'session-append :session-id session-id
                      :record (plist-get delta :record))))
          (should (<= (funcall measure actual-body)
                      (plist-get accounting :frame-reserve))))))))

(ert-deftest e-session-async-s92-maximum-command-fits-local-and-shared-caps ()
  "The real maximum create envelope is admissible; one-over stops pre-frame."
  (let* ((directory (make-temp-file "e-session-max-accounting-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (runtime (e-session-storage-runtime-store store))
         (limit 16777216)
         (exact (make-string limit ?x))
         (one-over (make-string (1+ limit) ?x))
         (measure (lambda (body)
                    (e-session-storage-measure-frame-escrow
                     store 'write body)))
         frame-called accounting)
    (unwind-protect
        (progn
          (setq accounting
                (e-session-aggregate-command-accounting
                 'create "maximum" (list :metadata (list :project-root exact))
                 measure))
          (should (= (plist-get accounting :producer-bytes) limit))
          (should (= (plist-get accounting :local-bytes)
                     e-session-async-lane-byte-capacity))
          (should
           (<= (+ (plist-get accounting :local-bytes)
                  (plist-get accounting :frame-reserve))
               (e-runtime-store--reservation-limit
                (e-runtime-store--reservation runtime))))
          (should-error
           (e-session-aggregate-command-accounting
            'create "maximum"
            (list :metadata (list :project-root one-over))
            (lambda (_body) (setq frame-called t) 0))
           :type 'e-session-command-too-large)
          (should-not frame-called))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-freeze-preserves-shared-string-aliases ()
  "P charges a shared string once and the detached graph preserves its alias."
  (let* ((store (e-session-store-create))
         (_session (e-session-aggregate-create store :id "alias"))
         (leaf (copy-sequence "shared-leaf"))
         (arguments
          (list :message
                (list :role 'user :content leaf
                      :metadata (list :same leaf))))
         (command (e-session-async-test--seal
                   store 'append-message "alias" arguments))
         (frozen-message
          (plist-get (e-session-aggregate-command-arguments command) :message))
         (frozen-content (plist-get frozen-message :content))
         (frozen-alias (plist-get (plist-get frozen-message :metadata) :same)))
    (should (= (plist-get (e-session-aggregate-command-account command)
                          :producer-bytes)
               (string-bytes leaf)))
    (should (eq frozen-content frozen-alias))
    (should-not (eq frozen-content leaf))
    (aset leaf 0 ?X)
    (should (equal frozen-content "shared-leaf"))))

(ert-deftest e-session-async-s92-command-create-append-fifo-forward-and-reopen ()
  "Queued create and appends interpret FIFO and replay to the same projection."
  (let* ((directory (make-temp-file "e-session-command-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (create (e-session-create store :id "fifo"))
         (first (e-session-append-message
                 store "fifo" '(:role user :content "first")))
         (second (e-session-append-message
                  store "fifo" '(:role assistant :content "second")))
         before-close)
    (unwind-protect
        (progn
          (dolist (work (list create first second))
            (should (e-work-handle-p work)))
          (should-not (e-session-aggregate-session-present-p store "fifo"))
          (dolist (work (list create first second))
            (e-session-async-test--wait-finished work))
          (let ((session (e-session-get store "fifo")))
            (should (equal (mapcar (lambda (message)
                                     (plist-get message :content))
                                   (plist-get session :messages))
                           '("first" "second")))
            (should (= (plist-get session :message-count) 2))
            (should (equal (plist-get session :summary) "first"))
            (should (equal (plist-get session :last-message-at)
                           (plist-get (cadr (plist-get session :messages))
                                      :created-at)))
            (should (equal (plist-get session :latest-assistant-marker)
                           (plist-get (cadr (plist-get session :messages)) :id)))
            (setq before-close
                  (list (mapcar (lambda (message)
                                  (list (plist-get message :id)
                                        (plist-get message :role)
                                        (plist-get message :content)
                                        (plist-get message :parent-id)))
                                (plist-get session :messages))
                        (plist-get session :message-count)
                        (plist-get session :summary)
                        (plist-get session :last-message-at)
                        (plist-get session :latest-assistant-marker)
                        (plist-get session :current-head-id)
                        (plist-get session :board-output-sequence)
                        (plist-get session :file)
                        (e-session-display-title store "fifo"))))
          (e-session-async-test--assert-zero-ownership store)
          (e-session-async-test--close store)
          (setq store (e-session-sqlite-store-create directory :load-all t))
          (let ((session (e-session-get store "fifo")))
            (should
             (equal before-close
                    (list (mapcar (lambda (message)
                                    (list (plist-get message :id)
                                          (plist-get message :role)
                                          (plist-get message :content)
                                          (plist-get message :parent-id)))
                                  (plist-get session :messages))
                          (plist-get session :message-count)
                          (plist-get session :summary)
                          (plist-get session :last-message-at)
                          (plist-get session :latest-assistant-marker)
                          (plist-get session :current-head-id)
                          (plist-get session :board-output-sequence)
                          (plist-get session :file)
                          (e-session-display-title store "fifo"))))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-generated-create-id-follows-reservation ()
  "A generated session identity is allocated only after all admission bytes."
  (let* ((directory (make-temp-file "e-session-command-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         observed-id-state
         work session)
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-identity-generate-id)
                   (lambda ()
                     (let* ((coordinator (e-session-async-coordinator store))
                            (runtime (e-session-storage-runtime-store store)))
                       (setq observed-id-state
                             (list
                              (e-session-async--coordinator-count coordinator)
                              (e-session-async--coordinator-bytes coordinator)
                              (e-runtime-store--reservation-used
                               (e-runtime-store--reservation runtime)))))
                     "generated-after-reserve")))
          (setq work (e-session-create store))
          (should (equal observed-id-state
                         (list 1
                               (e-session-async--coordinator-bytes
                                (e-session-async-coordinator store))
                               (e-runtime-store--reservation-used
                                (e-runtime-store--reservation
                                 (e-session-storage-runtime-store store))))))
          (setq session (e-session-async-test--wait-finished work))
          (should (equal (plist-get session :id) "generated-after-reserve"))
          (should (eq session (e-session-get store "generated-after-reserve")))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-command-admission-freezes-before-publication ()
  "Caller mutation after admission changes neither held delta nor live state."
  (let* ((directory (make-temp-file "e-session-command-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (text (copy-sequence "before"))
         (vector (vector text))
         (metadata (list :capability-state
                         (list :owner (list :text text :vector vector))))
         callback retained-body work)
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-storage-submit)
                   (lambda (owner _kind body settle &optional escrow)
                     (e-session-storage-release-frame-escrow owner escrow)
                     (setq retained-body body callback settle)
                     t)))
          (setq work (e-session-create store :id "freeze" :metadata metadata))
          (aset text 0 ?X)
          (aset vector 0 "changed")
          (let ((deadline (+ (float-time) 2.0)))
            (while (and (null callback) (< (float-time) deadline))
              (sit-for 0.01)))
          (should callback)
          (should-not (e-session-aggregate-session-present-p store "freeze"))
          (let* ((record (plist-get retained-body :record))
                 (state (plist-get (plist-get record :metadata)
                                   :capability-state))
                 (owner (plist-get state :owner)))
            (should (equal (plist-get owner :text) "before"))
            (should (equal (aref (plist-get owner :vector) 0) "before")))
          (funcall callback '(:revision 1) nil)
          (e-session-async-test--wait-finished work)
          (let* ((live (e-session-get store "freeze"))
                 (owner (plist-get (plist-get (plist-get live :metadata)
                                              :capability-state)
                                   :owner)))
            (should (equal (plist-get owner :text) "before"))
            (should (equal (aref (plist-get owner :vector) 0) "before")))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-activity-continuity-uses-frozen-command ()
  "Activity continuity is derived only from the sealed command graph."
  (let* ((directory (make-temp-file "e-session-command-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (turn-id (copy-sequence "turn-before"))
         (tool-name (copy-sequence "inspect-before"))
         (payload (list :tool-call (list :id "call-1" :name tool-name)))
         callback retained-body work)
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "activity-freeze"))
          (cl-letf (((symbol-function 'e-session-storage-submit)
                     (lambda (owner _kind body settle &optional escrow)
                       (e-session-storage-release-frame-escrow owner escrow)
                       (setq retained-body body callback settle)
                       t)))
            (setq work
                  (e-session-append-activity-event
                   store "activity-freeze" turn-id 'tool-started payload))
            (aset turn-id 0 ?X)
            (aset tool-name 0 ?X)
            (let ((deadline (+ (float-time) 2.0)))
              (while (and (null callback) (< (float-time) deadline))
                (sit-for 0.01)))
            (should callback)
            (let* ((continuity (plist-get retained-body :continuity))
                   (continuity-payload (plist-get continuity :payload)))
              (should (equal (plist-get continuity-payload :turn-id)
                             "turn-before"))
              (should (equal (plist-get continuity-payload :tool-name)
                             "inspect-before")))
            (funcall callback '(:revision 2) nil)
            (let ((entry (e-session-async-test--wait-finished work)))
              (should (equal (plist-get entry :turn-id) "turn-before"))
              (should (equal (plist-get
                              (plist-get (plist-get entry :payload) :tool-call)
                              :name)
                             "inspect-before"))))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-command-reserves-before-seal-and-rolls-back ()
  "Seal failure sees complete reservations and releases both ledgers exactly."
  (let* ((directory (make-temp-file "e-session-command-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         observed)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'e-session-aggregate-command-seal)
                     (lambda (&rest _arguments)
                       (let* ((coordinator (e-session-async-coordinator store))
                              (runtime (e-session-storage-runtime-store store)))
                         (setq observed
                               (list (e-session-async--coordinator-count coordinator)
                                     (e-session-async--coordinator-bytes coordinator)
                                     (e-runtime-store--reservation-used
                                      (e-runtime-store--reservation runtime)))))
                       (signal 'e-session-error '("injected seal failure")))))
            (should-error
             (e-session-async-submit-command
              store "seal" 'create '(:metadata nil))
             :type 'e-session-error))
          (should (= (car observed) 1))
          (should (> (cadr observed) 0))
          (should (> (caddr observed) 0))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-command-escrow-refusal-releases-every-owner ()
  "A storage refusal before transfer fails once with exact zero ownership."
  (let* ((directory (make-temp-file "e-session-command-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         work)
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-storage-submit)
                   (lambda (&rest _arguments) nil)))
          (setq work (e-session-create store :id "refused"))
          (should (eq (plist-get (e-session-async-test--wait work) :state)
                      'failed))
          (should-not (e-session-aggregate-session-present-p store "refused"))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-success-notifies-after-scheduler-release ()
  "Authoritative apply and lane release precede an isolated success observer."
  (let* ((directory (make-temp-file "e-session-command-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (work (e-session-create store :id "observer-release"))
         (operation (e-work-handle-arguments work))
         observed)
    (unwind-protect
        (progn
          (setf (e-work-handle-callbacks work)
                (plist-put
                 (e-work-handle-callbacks work) :on-done
                 (lambda (session)
                   (let ((coordinator (e-session-async-coordinator store)))
                     (setq observed
                           (list
                            (eq session (e-session-get store "observer-release"))
                            (e-session-async--coordinator-count coordinator)
                            (e-session-async--coordinator-bytes coordinator)
                            (e-session-async--operation-lane operation)
                            (e-session-async--operation-coordinator operation)
                            (e-work-handle-arguments work))))
                   (error "isolated success observer"))))
          (e-session-async-test--wait-finished work)
          (should (equal observed '(t 0 0 nil nil nil)))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-completion-actions-retry-without-reapply ()
  "Every real post-apply action retains its outbox owner across error and quit."
  (dolist (fault-case
           (cons '(apply-after-return after)
                 (cl-loop for action in '(projection result shared-release
                                          lane-detach schedule terminal-enqueue)
                          append (list (list action 'before)
                                       (list action 'after)))))
    (pcase-let ((`(,fault-action ,fault-edge) fault-case))
    (let* ((directory (make-temp-file "e-session-post-apply-" t))
           (store (e-session-sqlite-store-create directory :asynchronous t))
           (session-id (format "completion-%s-%s" fault-action fault-edge))
           (original-apply
            (symbol-function 'e-session-aggregate-apply-committed-record))
           (apply-count 0)
           injected retained-head token-retained marker-seen
           first second first-operation)
      (unwind-protect
          (progn
            (e-session-async-test--wait-finished
             (e-session-create store :id session-id))
            (let ((e-session-async--completion-fault-function
                   (lambda (action edge)
                     (when (and (eq action fault-action)
                                (eq edge fault-edge)
                                (not injected))
                       (setq injected t)
                       (let* ((operation first-operation)
                              (coordinator
                               (e-session-async-coordinator store)))
                         (setq retained-head
                               (eq operation
                                   (car (e-session-async--coordinator-publication-outbox
                                         coordinator)))
                               token-retained
                               (e-session-async--operation-terminal-token operation)
                               marker-seen
                               (e-session-async--operation-applied operation)))
                       (if (cl-evenp
                            (cl-position fault-case
                                         (cons '(apply-after-return after)
                                               (cl-loop
                                                for item in
                                                '(projection result shared-release
                                                  lane-detach schedule
                                                  terminal-enqueue)
                                                append
                                                (list (list item 'before)
                                                      (list item 'after))))
                                         :test #'equal))
                           (error "post-apply boundary error")
                         (signal 'quit nil))))))
              (cl-letf (((symbol-function
                          'e-session-aggregate-apply-committed-record)
                         (lambda (owner record)
                           (cl-incf apply-count)
                           (funcall original-apply owner record))))
                (setq first
                      (e-session-append-message
                       store session-id '(:role user :content "first"))
                      second
                      (e-session-append-message
                       store session-id '(:role assistant :content "second")))
                (setq first-operation (e-work-handle-arguments first))
                (should (equal (plist-get
                                (e-session-async-test--wait-finished first)
                                :content)
                               "first"))
                (should (equal (plist-get
                                (e-session-async-test--wait-finished second)
                                :content)
                               "second"))))
            (should injected)
            (should retained-head)
            (should token-retained)
            (should marker-seen)
            (should (= apply-count 2))
            (should
             (equal (mapcar (lambda (message) (plist-get message :content))
                            (e-session-messages store session-id))
                    '("first" "second")))
            (should-not (e-session-async-reconciliation-required-p
                         store session-id))
            (e-session-async-test--assert-zero-ownership store))
        (e-session-async-test--close store)
        (delete-directory directory t))))))

(ert-deftest e-session-async-s92-token-preallocation-failure-rolls-back-admission ()
  "Token allocation faults retain no reservation and a later admission succeeds."
  (dolist (edge '(before after))
    (let* ((directory (make-temp-file "e-session-token-allocation-" t))
           (store (e-session-sqlite-store-create directory :asynchronous t))
           (session-id (format "token-%s" edge))
           injected)
      (unwind-protect
          (progn
            (let ((e-session-async--completion-fault-function
                   (lambda (action at)
                     (when (and (eq action 'token-allocation) (eq at edge))
                       (setq injected t)
                       (error "token allocation fault")))))
              (should-error (e-session-create store :id session-id)
                            :type 'error))
            (should injected)
            (should-not (e-session-aggregate-session-present-p store session-id))
            (e-session-async-test--assert-zero-ownership store)
            (should (eq (e-session-async-test--wait-finished
                         (e-session-create store :id session-id))
                        (e-session-get store session-id)))
            (e-session-async-test--assert-zero-ownership store))
        (e-session-async-test--close store)
        (delete-directory directory t)))))

(ert-deftest e-session-async-s92-persistent-projection-mark-failure-is-secondary ()
  "A persistent projection mark exit never gates success or its FIFO tail."
  (dolist (mark-exit '(error quit))
    (let* ((directory (make-temp-file "e-session-projection-mark-" t))
           (store (e-session-sqlite-store-create directory :asynchronous t))
           (session-id (format "projection-mark-%s" mark-exit))
           (original-apply
            (symbol-function 'e-session-aggregate-apply-committed-record))
           (apply-count 0)
           (mark-count 0)
           (note-count 0))
      (unwind-protect
          (progn
            (e-session-async-test--wait-finished
             (e-session-create store :id session-id))
            (cl-letf (((symbol-function 'e-session-storage--mark-checkpoint-dirty)
                       (lambda (&rest _)
                         (cl-incf mark-count)
                         (if (eq mark-exit 'quit)
                             (signal 'quit nil)
                           (error "persistent projection mark"))))
                      ((symbol-function 'e-session-storage-note-projection-error)
                       (lambda (&rest _) (cl-incf note-count)))
                      ((symbol-function 'e-session-aggregate-apply-committed-record)
                       (lambda (owner record)
                         (cl-incf apply-count)
                         (funcall original-apply owner record))))
              (let ((first (e-session-append-message
                            store session-id '(:role user :content "first")))
                    (second (e-session-append-message
                             store session-id '(:role assistant :content "second"))))
                (should (equal (plist-get
                                (e-session-async-test--wait-finished first)
                                :content)
                               "first"))
                (should (equal (plist-get
                                (e-session-async-test--wait-finished second)
                                :content)
                               "second"))))
            ;; Allow any accidentally armed zero-delay retry to run before the
            ;; bounded counts and timer ownership are inspected.
            (sit-for 0.05)
            (let ((coordinator (e-session-async-coordinator store)))
              (should (= apply-count 2))
              (should (= mark-count 2))
              (should (= note-count 2))
              (should-not (e-session-async--coordinator-publication-outbox
                           coordinator))
              (should-not (e-session-async--coordinator-terminal-outbox
                           coordinator))
              (should-not (timerp
                           (e-session-async--coordinator-publication-timer
                            coordinator)))
              (should-not (timerp
                           (e-session-async--coordinator-terminal-timer
                            coordinator)))
              (should-not (timerp
                           (e-session-async--coordinator-scheduler-timer
                            coordinator))))
            (should-not (e-session-async-reconciliation-required-p
                         store session-id))
            (e-session-async-test--assert-zero-ownership store))
        (e-session-async-test--close store)
        (delete-directory directory t)))))

(ert-deftest e-session-async-s92-persistent-projection-diagnostic-failure-is-secondary ()
  "A persistent diagnostic exit is isolated after each one-shot mark failure."
  (dolist (diagnostic-exit '(error quit))
    (let* ((directory (make-temp-file "e-session-projection-note-" t))
           (store (e-session-sqlite-store-create directory :asynchronous t))
           (session-id (format "projection-note-%s" diagnostic-exit))
           (original-apply
            (symbol-function 'e-session-aggregate-apply-committed-record))
           (apply-count 0)
           (mark-count 0)
           (note-count 0))
      (unwind-protect
          (progn
            (e-session-async-test--wait-finished
             (e-session-create store :id session-id))
            (cl-letf (((symbol-function 'e-session-storage--mark-checkpoint-dirty)
                       (lambda (&rest _)
                         (cl-incf mark-count)
                         (error "persistent projection mark")))
                      ((symbol-function 'e-session-storage-note-projection-error)
                       (lambda (&rest _)
                         (cl-incf note-count)
                         (if (eq diagnostic-exit 'quit)
                             (signal 'quit nil)
                           (error "persistent projection diagnostic"))))
                      ((symbol-function 'e-session-aggregate-apply-committed-record)
                       (lambda (owner record)
                         (cl-incf apply-count)
                         (funcall original-apply owner record))))
              (let ((first (e-session-append-message
                            store session-id '(:role user :content "first")))
                    (second (e-session-append-message
                             store session-id '(:role assistant :content "second"))))
                (should (equal (plist-get
                                (e-session-async-test--wait-finished first)
                                :content)
                               "first"))
                (should (equal (plist-get
                                (e-session-async-test--wait-finished second)
                                :content)
                               "second"))))
            (sit-for 0.05)
            (let ((coordinator (e-session-async-coordinator store)))
              (should (= apply-count 2))
              (should (= mark-count 2))
              (should (= note-count 2))
              (should-not (e-session-async--coordinator-publication-outbox
                           coordinator))
              (should-not (e-session-async--coordinator-terminal-outbox
                           coordinator))
              (should-not (timerp
                           (e-session-async--coordinator-publication-timer
                            coordinator)))
              (should-not (timerp
                           (e-session-async--coordinator-terminal-timer
                            coordinator)))
              (should-not (timerp
                           (e-session-async--coordinator-scheduler-timer
                            coordinator))))
            (should-not (e-session-async-reconciliation-required-p
                         store session-id))
            (e-session-async-test--assert-zero-ownership store))
        (e-session-async-test--close store)
        (delete-directory directory t)))))

(ert-deftest e-session-async-s92-admission-escrow-exhaustion-is-terminal-work ()
  "Composition refusal rolls admission back and returns one failed work."
  (let* ((directory (make-temp-file "e-session-command-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         work)
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-storage-reserve-frame-escrow)
                   (lambda (&rest _arguments)
                     (signal 'e-session-async-capacity-exhausted
                             '("injected composition exhaustion")))))
          (setq work (e-session-create store :id "capacity"))
          (should (e-work-handle-p work))
          (should (eq (plist-get (e-session-async-test--wait work) :state)
                      'failed))
          (should-not (e-session-aggregate-session-present-p store "capacity"))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-command-grammar-covers-migrated-facades ()
  "Every command tag and session-info field in this slice commits via work."
  (let* ((directory (make-temp-file "e-session-command-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (session-id "grammar"))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id session-id
                             :metadata '(:project-root "/initial/")))
          (let ((entry
                 (e-session-async-test--wait-finished
                  (e-session-append-activity-event
                   store session-id "turn" 'note '(:value 1)))))
            (should (eq (plist-get entry :event-type) 'note)))
          (let ((entry
                 (e-session-async-test--wait-finished
                  (e-session-append-context-curation-response
                   store session-id "turn" "response-entry"))))
            (should (equal (plist-get entry :id) "response-entry")))
          (let ((message
                 (e-session-async-test--wait-finished
                  (e-session-append-message
                   store session-id '(:id "display-me" :role user
                                      :content "visible")))))
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
           (equal (plist-get
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
                 (first-package
                  (e-session-async-test--wait-finished
                   (e-session-append-context-curation-package
                    store session-id package)))
                 (submits 0)
                 duplicate)
            (should (plist-member first-package :record))
            (cl-letf (((symbol-function 'e-session-storage-submit)
                       (lambda (&rest _arguments)
                         (cl-incf submits)
                         (ert-fail "duplicate package submitted storage"))))
              (setq duplicate
                    (e-session-async-test--wait-finished
                     (e-session-append-context-curation-package
                      store session-id package))))
            (should (plist-get duplicate :already-present))
            (should-not (plist-member duplicate :record))
            (should (= submits 0)))
          (let* ((envelope (list :id "board-message" :kind 'output
                                 :content "detached"))
                 (returned
                  (e-session-async-test--wait-finished
                   (e-session-append-board-message
                    store session-id envelope)))
                 (submits 0)
                 duplicate)
            (setf (plist-get envelope :content) "mutated")
            (should (equal (plist-get returned :content) "detached"))
            (cl-letf (((symbol-function 'e-session-storage-submit)
                       (lambda (&rest _arguments)
                         (cl-incf submits)
                         (ert-fail "duplicate board envelope submitted"))))
              (setq duplicate
                    (e-session-async-test--wait-finished
                     (e-session-append-board-message
                      store session-id returned))))
            (should (equal duplicate returned))
            (should-not (eq duplicate returned))
            (should (= submits 0)))
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
           (equal
            (e-session-async-test--wait-finished
             (e-session-set-metadata store session-id '(:project-root "/meta/")))
            '(:project-root "/meta/")))
          (should
           (equal
            (e-session-async-test--wait-finished
             (e-session-set-session-config store session-id '(:model "model-a")))
            '(:project-root "/meta/" :model "model-a")))
          (let ((references
                 '(:attachments ((:uri "buffer://source" :id "source")))))
            (should
             (equal
              (e-session-async-test--wait-finished
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
           (equal
            (e-session-async-test--wait-finished
             (e-session-set-capability-state
              store session-id 'mcp '(:enabled t) :version 2))
            '(:version 2 :state (:enabled t))))
          (should
           (equal
            (e-session-async-test--wait-finished
             (e-session-set-turn-options store session-id '(:model "model-b")))
            '(:model "model-b")))
          (should
           (equal
            (e-session-async-test--wait-finished
             (e-session-set-current-branch store session-id "branch-a"))
            "branch-a"))
          (should
           (eq
            (e-session-async-test--wait-finished
             (e-session-rename store session-id "Command session"))
            (e-session-get store session-id)))
          (let* ((session (e-session-get store session-id))
                 (metadata (plist-get session :metadata)))
            (should (equal (plist-get metadata :project-root) "/meta/"))
            (should (equal (plist-get metadata :model) "model-a"))
            (should (equal (plist-get (plist-get metadata :org-canvas-ref) :uri)
                           "buffer://canvas"))
            (should (equal (e-session-capability-state store session-id 'mcp)
                           '(:version 2 :state (:enabled t))))
            (should (equal (e-session-turn-options store session-id)
                           '(:model "model-b")))
            (should (equal (plist-get session :current-branch) "branch-a"))
            (should (equal (plist-get session :name) "Command session"))
            (should (= (length (plist-get session :activity-events)) 2)))
          (e-session-async-test--assert-zero-ownership store)
          (e-session-async-test--close store)
          (setq store (e-session-sqlite-store-create directory :load-all t))
          (let ((session (e-session-get store session-id)))
            (should (equal (plist-get (plist-get session :metadata) :model)
                           "model-a"))
            (should (equal (plist-get session :turn-options)
                           '(:model "model-b")))
            (should (equal (plist-get session :current-branch) "branch-a"))
            (should (equal (plist-get session :name) "Command session"))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-capability-result-preserves-frozen-input-shape ()
  "Capability results preserve list/vector shape and exact version wrapping."
  (let* ((directory (make-temp-file "e-session-capability-result-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (legacy (e-session-store-create))
         (session-id "capability-result"))
    (unwind-protect
        (progn
          (e-session-create legacy :id session-id)
          (e-session-async-test--wait-finished
           (e-session-create store :id session-id))
          (dolist (case '((list-state nil) (vector-state 7)))
            (pcase-let ((`(,capability-id ,version) case))
              (let* ((state (if version
                                (vector (list :enabled t))
                              (list :enabled t)))
                     (expected-state (copy-tree state t))
                     (legacy-result
                      (e-session-set-capability-state
                       legacy session-id capability-id
                       (copy-tree state t) :version version))
                     callback work)
                (cl-letf (((symbol-function 'e-session-storage-submit)
                           (lambda (owner _kind _body settle &optional escrow)
                             (e-session-storage-release-frame-escrow owner escrow)
                             (setq callback settle)
                             t)))
                  (setq work
                        (e-session-set-capability-state
                         store session-id capability-id state :version version))
                  (if (vectorp state)
                      (aset state 0 'mutated)
                    (setcar state :mutated))
                  (let ((deadline (+ (float-time) 2.0)))
                    (while (and (null callback) (< (float-time) deadline))
                      (sit-for 0.01)))
                  (should callback)
                  (funcall callback '(:revision 7) nil)
                  (let ((result (e-session-async-test--wait-finished work)))
                    (should (equal result legacy-result))
                    (if version
                        (progn
                          (should (= (plist-get result :version) version))
                          (should (vectorp (plist-get result :state)))
                          (should (equal (plist-get result :state) expected-state)))
                      (progn
                        (should (listp result))
                        (should (equal result expected-state)))))))))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-clear-and-delete-are-durable-before-visible ()
  "Transcript clear and delete publish only after their durable ACKs."
  (let* ((directory (make-temp-file "e-session-control-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         callback body clear delete)
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "controls"))
          (e-session-async-test--wait-finished
           (e-session-append-message
            store "controls" '(:role user :content "before clear")))
          (cl-letf (((symbol-function 'e-session-storage-submit)
                     (lambda (owner _kind submitted settle &optional escrow)
                       (e-session-storage-release-frame-escrow owner escrow)
                       (setq body submitted callback settle)
                       t)))
            (setq clear (e-session-clear-messages store "controls"))
            (while (null callback) (sit-for 0.01))
            (should (equal (plist-get body :op) 'session-append))
            (should (= (length (e-session-messages store "controls")) 1))
            (funcall callback '(:revision 2) nil)
            (let ((result (e-session-async-test--wait-finished clear)))
              (should (eq (plist-get result :type) 'session-event))
              (should (eq (plist-get result :event-type) 'messages-cleared))))
          (should-not (e-session-messages store "controls"))
          (setq callback nil body nil)
          (cl-letf (((symbol-function 'e-session-storage-submit)
                     (lambda (owner _kind submitted settle &optional escrow)
                       (e-session-storage-release-frame-escrow owner escrow)
                       (setq body submitted callback settle)
                       t)))
            (setq delete (e-session-delete store "controls"))
            (while (null callback) (sit-for 0.01))
            (should (equal body '(:op session-delete :session-id "controls")))
            (should (e-session-session-present-p store "controls"))
            (funcall callback '(:revision 3) nil)
            (should (eq (e-session-async-test--wait-finished delete) t)))
          (should-not (e-session-session-present-p store "controls"))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-noop-display-submits-no-storage-request ()
  "A display request for a missing message retires without physical I/O."
  (let* ((directory (make-temp-file "e-session-noop-display-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (submits 0))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "noop-display"))
          (cl-letf (((symbol-function 'e-session-storage-submit)
                     (lambda (&rest _arguments)
                       (cl-incf submits)
                       (ert-fail "missing display submitted storage"))))
            (should-not
             (e-session-async-test--wait-finished
              (e-session-set-message-display
               store "noop-display" "missing" 'hidden))))
          (should (= submits 0))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-clear-and-delete-survive-reopen ()
  "Actual control transports restore clear state and permanently delete."
  (let* ((directory (make-temp-file "e-session-control-reopen-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t)))
    (unwind-protect
        (progn
          (dolist (id '("clear-reopen" "delete-reopen"))
            (e-session-async-test--wait-finished
             (e-session-create store :id id))
            (e-session-async-test--wait-finished
             (e-session-append-message
              store id '(:role user :content "temporary"))))
          (e-session-async-test--wait-finished
           (e-session-clear-messages store "clear-reopen"))
          (should
           (eq (e-session-async-test--wait-finished
                (e-session-delete store "delete-reopen"))
               t))
          (e-session-async-test--assert-zero-ownership store)
          (e-session-async-test--close store)
          (setq store (e-session-sqlite-store-create directory :load-all t))
          (should-not (e-session-messages store "clear-reopen"))
          (should-not (e-session-session-present-p store "delete-reopen")))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-clear-context-and-board-rollback-owned-state ()
  "Faults restore transcript indexes and Board journal roots/tails exactly."
  (dolist (tag '(clear-messages context-curation-package board-message
                 board-messages-clear board-state delete))
    (let* ((store (e-session-store-create))
           (_session (e-session-aggregate-create store :id "rollback-family"))
           (_message (e-session-aggregate-append-message
                      store "rollback-family"
                      '(:id "seed" :role user :content "seed")))
           (_board (e-session-aggregate-append-board-message
                    store "rollback-family"
                    '(:id "board-seed" :kind output :content "seed")))
           (_generation
            (e-session-aggregate-append-context-generation
             store "rollback-family"
             (e-context-lifetime-generation-create
              :id "rollback-generation" :checkpoint nil
              :covered-session-boundary
              (plist-get (e-session-get store "rollback-family")
                         :root-event-id))))
           (arguments
            (pcase tag
              ('clear-messages nil)
              ('context-curation-package
               '(:package
                 (:promotion
                  (:record-version 3 :type context-promotion
                   :id "rollback-curation" :frame-id "rollback-frame"
                   :generation-id "rollback-generation"
                   :consumer-request-id "rollback-consumer"
                   :response-entry-id "rollback-response"
                   :items ((:kind exact :value "kept"
                            :source-observation-ids ("rollback-observation")
                            :source-refs ("rollback-source")
                            :source-fingerprints ("rollback-fingerprint"))))
                  :erasure nil)))
              ('board-message
               '(:message (:id "board-new" :kind output :content "new")))
              ('board-messages-clear nil)
              ('board-state
               '(:principal "principal" :board-id "board"
                 :association-role "owner" :routing-policy nil))
              ('delete nil)))
           (command (e-session-async-test--seal
                     store tag "rollback-family" arguments))
           (delta (e-session-aggregate-command-interpret store command))
           (session (e-session-get store "rollback-family"))
           (index (gethash "rollback-family"
                           (e-session-store-entry-indexes store)))
           (journal (gethash "rollback-family"
                             (e-session-store-board-journals store)))
           (before-session (prin1-to-string session))
           (before-index-count (hash-table-count index))
           (before-board
            (prin1-to-string
             (e-session-board-journal-messages journal)))
           (before-tail (e-session-board-journal-tail journal)))
      (dolist (boundary
               (pcase tag
                 ((or 'board-message 'board-messages-clear 'board-state)
                  '(after-list-state after-derived))
                 ('delete '(after-list-state after-derived after-index))
                 (_ '(after-list-state after-derived after-index))))
        (let ((e-session-aggregate--committed-apply-fault-function
               (lambda (at)
                 (when (eq at boundary)
                   (signal 'e-session-error (list "injected" tag boundary))))))
          (should-error
           (e-session-aggregate-apply-committed-record
            store (plist-get delta :record))
           :type 'e-session-error))
        (should (eq (e-session-get store "rollback-family") session))
        (should (eq (gethash "rollback-family"
                             (e-session-store-entry-indexes store))
                    index))
        (should (eq (gethash "rollback-family"
                             (e-session-store-board-journals store))
                    journal))
        (should (equal (prin1-to-string session) before-session))
        (should (= (hash-table-count index) before-index-count))
        (should (equal
                 (prin1-to-string
                  (e-session-board-journal-messages journal))
                 before-board))
        (should (eq (e-session-board-journal-tail journal) before-tail))))))

(ert-deftest e-session-async-s92-session-info-persists-bounded-delta ()
  "Session-info transport excludes a complete resulting metadata projection."
  (let* ((directory (make-temp-file "e-session-command-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         retained-body callback work)
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "bounded-state"
                             :metadata '(:project-root "/old/")))
          (cl-letf (((symbol-function 'e-session-storage-submit)
                     (lambda (owner _kind body settle &optional escrow)
                       (e-session-storage-release-frame-escrow owner escrow)
                       (setq retained-body body callback settle)
                       t)))
            (setq work
                  (e-session-set-session-config
                   store "bounded-state" '(:model "new-model")))
            (let ((deadline (+ (float-time) 2.0)))
              (while (and (null callback) (< (float-time) deadline))
                (sit-for 0.01)))
            (let ((record (plist-get retained-body :record)))
              (should (eq (plist-get record :field) 'config))
              (should (equal (plist-get record :value) '(:model "new-model")))
              (should-not (plist-member record :metadata)))
            (funcall callback '(:revision 2) nil)
            (should
             (equal (e-session-async-test--wait-finished work)
                    '(:project-root "/old/" :model "new-model"))))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-tool-continuity-shares-append-transaction ()
  "Async tool messages and activity persist their continuity cut points."
  (let* ((directory (make-temp-file "e-session-command-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (session-id "tool-composite"))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id session-id))
          (e-session-async-test--wait-finished
           (e-session-append-message
            store session-id
            '(:role tool-call
              :content (:id "call-1" :name "inspect" :arguments (:x 1)))))
          (should
           (equal (mapcar (lambda (fact)
                            (cons (plist-get fact :call-id)
                                  (plist-get fact :state)))
                          (e-session-tool-followup-classifications
                           store session-id))
                  '(("call-1" . admitted))))
          (e-session-async-test--wait-finished
           (e-session-append-activity-event
            store session-id "turn-1" 'tool-started
            '(:tool-call (:id "call-1" :name "inspect"))))
          (should
           (equal (mapcar (lambda (fact)
                            (cons (plist-get fact :call-id)
                                  (plist-get fact :state)))
                          (e-session-tool-followup-classifications
                           store session-id))
                  '(("call-1" . claimed))))
          (e-session-async-test--wait-finished
           (e-session-append-activity-event
            store session-id "turn-1" 'tool-finished
            '(:tool-call (:id "call-1" :name "inspect")
              :result (:status ok))))
          (should
           (equal (mapcar (lambda (fact)
                            (cons (plist-get fact :call-id)
                                  (plist-get fact :state)))
                          (e-session-tool-followup-classifications
                           store session-id))
                  '(("call-1" . resulted))))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-committed-apply-rolls-back-every-boundary ()
  "Every injected committed-apply fault leaves old lists/index/sequence intact."
  (let* ((store (e-session-store-create))
         (_session (e-session-aggregate-create store :id "rollback"))
         (_first (e-session-aggregate-append-message
                  store "rollback" '(:role user :content "old")))
         (arguments '(:message (:role assistant :content "new")))
         (command (e-session-async-test--seal
                   store 'append-message "rollback" arguments))
         (delta (e-session-aggregate-command-interpret store command))
         (old-session (e-session-get store "rollback"))
         (old-index (gethash "rollback" (e-session-store-entry-indexes store)))
         (old-journal (e-session-aggregate--board-journal-create))
         (old-message (car (plist-get old-session :messages)))
         (old-head (plist-get old-session :current-head-id))
         (old-sequence (e-session-store-sequence store)))
    (puthash "rollback" old-journal (e-session-store-board-journals store))
    (dolist (boundary '(before-record after-record after-list-state
                       after-derived after-index))
      (let ((e-session-aggregate--committed-apply-fault-function
             (lambda (at)
               (when (eq at boundary)
                 (signal 'e-session-error (list "injected" boundary))))))
        (should-error
         (e-session-aggregate-apply-committed-record
          store (plist-get delta :record))
         :type 'e-session-error))
      (let ((session (e-session-get store "rollback")))
        (should (eq session old-session))
        (should (eq (gethash "rollback" (e-session-store-entry-indexes store))
                    old-index))
        (should (eq (gethash "rollback" (e-session-store-board-journals store))
                    old-journal))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :content))
                               (plist-get session :messages))
                       '("old")))
        (should (eq (car (plist-get session :messages)) old-message))
        (should (equal (plist-get session :current-head-id) old-head))
        (should-not (e-session-entry-by-id
                     store "rollback" (plist-get (plist-get delta :record) :id)))
        (should (= (e-session-store-sequence store) old-sequence))))
    (e-session-aggregate-apply-committed-record store (plist-get delta :record))
    (should (equal (mapcar (lambda (message) (plist-get message :content))
                           (e-session-messages store "rollback"))
                   '("old" "new")))))

(ert-deftest e-session-async-s92-exhaustion-detaches-32-before-observers ()
  "Reconciliation retains proof, then detaches/releases a full lane atomically."
  (let* ((directory (make-temp-file "e-session-reconcile-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (runtime (e-session-storage-runtime-store store))
         storage-callback works operation first-delta
         (attempts 0) (observer-count 0) observer-saw-detached
         drain-pages (heartbeat 0) heartbeat-timer)
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "reconcile"))
          (cl-letf (((symbol-function 'e-session-storage-submit)
                     (lambda (owner _kind _body settle &optional escrow)
                       (e-session-storage-release-frame-escrow owner escrow)
                       (setq storage-callback settle)
                       t)))
            (dotimes (index 32)
              (let ((observer-index index)
                    (work
                     (e-session-append-message
                      store "reconcile"
                      (list :role 'user :content (format "tail-%02d" index)))))
                (setf (e-work-handle-callbacks work)
                      (plist-put
                       (e-work-handle-callbacks work) :on-error
                       (lambda (_cause)
                         (let* ((coordinator (e-session-async-coordinator store))
                                (lane (gethash
                                       "reconcile"
                                       (e-session-async--coordinator-lanes
                                        coordinator))))
                           (push (and lane
                                      (null (e-session-async--lane-active lane))
                                      (null (e-session-async--lane-queue lane)))
                                 observer-saw-detached))
                         (cl-incf observer-count)
                         (pcase observer-index
                           (0 (e-session-async--drain-terminals
                               (e-session-async-coordinator store)))
                           (1 (error "observer error"))
                           (2 (signal 'quit nil))
                           (3 (throw 'observer-escape t))))))
                (push work works)))
            (setq works (nreverse works))
            (let ((deadline (+ (float-time) 2.0)))
              (while (and (null storage-callback) (< (float-time) deadline))
                (sit-for 0.01)))
            (should storage-callback)
            (setq operation (e-work-handle-arguments (car works))
                  first-delta (e-session-async--operation-delta operation)
                  heartbeat-timer
                  (run-at-time 0 0.001 (lambda () (cl-incf heartbeat))))
            (let ((e-session-aggregate--committed-apply-fault-function
                   (lambda (boundary)
                     (when (eq boundary 'before-record)
                       (cl-incf attempts)
                       (should (eq (e-session-async--operation-delta operation)
                                   first-delta))
                       (error "first aggregate invariant"))))
                  (original-drain
                   (symbol-function 'e-session-async--drain-terminals)))
              (cl-letf (((symbol-function 'e-session-async--drain-terminals)
                         (lambda (coordinator)
                           (unless (e-session-async--coordinator-terminal-draining
                                    coordinator)
                             (push (length
                                    (e-session-async--coordinator-terminal-outbox
                                     coordinator))
                                   drain-pages))
                           ;; Prove a genuine nonlocal observer exit, not only
                           ;; the `no-catch' error produced without a catcher.
                           (catch 'observer-escape
                             (funcall original-drain coordinator)))))
                (funcall storage-callback '(:revision 9) nil)
                (let ((deadline (+ (float-time) 5.0)))
                  (while (and (seq-some
                               (lambda (work)
                                 (not (e-request-terminal-p
                                       (e-work-handle-lifecycle work))))
                               works)
                              (< (float-time) deadline))
                    (sit-for 0.01)))))
            (cancel-timer heartbeat-timer)
            (setq heartbeat-timer nil)
            (should (= attempts 3))
            (should (= observer-count 32))
            (should (seq-every-p #'identity observer-saw-detached))
            (should (equal (nreverse drain-pages) '(32 28 12)))
            (should (> heartbeat 0))
            (dolist (work works)
              (should (eq (plist-get (e-work-status work) :state) 'failed))
              (should-not (e-work-handle-arguments work)))
            (let* ((coordinator (e-session-async-coordinator store))
                   (lane (gethash "reconcile"
                                  (e-session-async--coordinator-lanes coordinator)))
                   (barrier (e-session-async--lane-reconciliation lane)))
              (should (= (e-session-async--lane-count lane) 1))
              (should (= (e-session-async--lane-bytes lane) 2048))
              (should (= (e-session-async--coordinator-count coordinator) 1))
              (should (= (e-session-async--coordinator-bytes coordinator) 2048))
              (should (= (e-session-async--coordinator-reconciliation-bytes
                          coordinator) 2048))
              (should (memq (plist-get barrier :cause-kind)
                            e-session-async-reconciliation-cause-kinds))
              (should (<= (string-bytes (plist-get barrier :diagnostic)) 1024))
              (should (e-session-async-reconciliation-match-p
                       store "reconcile"
                       (plist-get barrier :request-id)
                       (plist-get barrier :delta-id) 9))
              (should-not (e-session-async-reconciliation-match-p
                           store "reconcile" "wrong"
                           (plist-get barrier :delta-id) 9))
              (should-not (plist-member barrier :delta)))
            (should (= (e-runtime-store--reservation-used
                        (e-runtime-store--reservation runtime)) 2048))
            (e-session-async-teardown store)
            (should (= (e-runtime-store--reservation-used
                        (e-runtime-store--reservation runtime)) 0))))
      (when (timerp heartbeat-timer) (cancel-timer heartbeat-timer))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-closed-command-surface-has-no-stage-escape ()
  "The async coordinator exposes neither generic mutation nor projection state."
  (should-not (fboundp 'e-session-async-submit))
  (should-not (fboundp 'e-session-async-submit-mutation))
  (should-not (fboundp 'e-session-async--lane-projected-stage))
  (should-not (fboundp 'e-session-async--lane-projection-bytes))
  (should (equal e-session-aggregate-command-tags
                 '(create append-message append-activity
                   context-curation-response message-display process-report
                   branch-summary compaction provider-anchor
                   context-generation context-curation-package clear-messages
                   board-message board-state board-messages-clear delete
                   session-info))))

(ert-deftest e-session-async-s92-interpreter-never-stages-a-session ()
  "Lane-head interpretation reads committed state without a session clone."
  (let* ((store (e-session-store-create))
         (_session (e-session-aggregate-create store :id "direct-delta"))
         (arguments '(:message (:role user :content "direct")))
         (command (e-session-async-test--seal
                   store 'append-message "direct-delta" arguments))
         delta)
    (cl-letf (((symbol-function 'e-session-aggregate-stage-session-mutation)
               (lambda (&rest _arguments)
                 (ert-fail "sealed interpreter attempted whole-session staging"))))
      (setq delta (e-session-aggregate-command-interpret store command)))
    (should (equal (plist-get (plist-get delta :record) :type) "message"))
    (should-not (e-session-messages store "direct-delta"))))

(ert-deftest e-session-async-s92-streaming-package-hash-preserves-legacy-id ()
  "Bounded streaming SHA matches the historical materialized printer bytes."
  (dolist (value (list nil
                       '("ascii" :value 3)
                       '("météo" (:city "中"))
                       (list :payload (make-string 8192 ?x))))
    (should
     (equal (e-session-aggregate--sha256-prin1 value)
            (secure-hash 'sha256 (prin1-to-string value))))))

(ert-deftest e-session-async-s92-control-producer-bounds-are-exact ()
  "The control family enforces its 64 KiB and producer-node remainder."
  (let* ((spec (cdr (e-session-aggregate-command-family-spec 'control)))
         (node-limit (- (plist-get spec :nodes)
                        (e-session-aggregate-command-fixed-nodes 'control)))
         (exact-nodes (e-session-async-test--balanced-cons-tree node-limit))
         (over-nodes (e-session-async-test--balanced-cons-tree (1+ node-limit)))
         (exact-bytes (make-string 65536 ?x))
         (over-bytes (make-string 65537 ?x)))
    (should (= (plist-get
                (e-session-aggregate--command-measure-producer-graph
                 exact-nodes 65536 node-limit)
                :nodes)
               node-limit))
    (should-error
     (e-session-aggregate--command-measure-producer-graph
      over-nodes 65536 node-limit)
     :type 'e-session-command-too-large)
    (should (= (e-session-aggregate-command-measure-producer
                exact-bytes 65536)
               65536))
    (should-error
     (e-session-aggregate-command-measure-producer over-bytes 65536)
     :type 'e-session-command-too-large)))

(ert-deftest e-session-async-s92-every-command-tag-has-generated-frame-fixture ()
  "Every tag's generated Freserve also covers an interpreted physical body."
  (let* ((store (e-session-store-create))
         (_session (e-session-aggregate-create store :id "s"))
         (_message (e-session-aggregate-append-message
                    store "s" '(:id "m" :role user :content "existing")))
         (generation
          (e-context-lifetime-generation-create
           :id "generation" :checkpoint nil
           :covered-session-boundary (plist-get _session :root-event-id)))
         (_generation
          (e-session-aggregate-append-context-generation
           store "s" generation))
         (promotion
          '(:record-version 3 :type context-promotion :id "promotion"
            :frame-id "frame" :generation-id "generation"
            :consumer-request-id "consumer" :response-entry-id "response"
            :items ((:kind exact :value "kept"
                     :source-observation-ids ("observation")
                     :source-refs ("source")
                     :source-fingerprints ("fingerprint")))))
         (cases
          `((create nil (:metadata nil))
            (append-message "s" (:message (:id "m" :role user :content "x")))
            (append-activity "s" (:turn-id "t" :event-type note :payload nil))
            (context-curation-response "s" (:turn-id "t" :response-entry-id "r"))
            (message-display "s" (:message-id "m" :display hidden))
            (process-report "s" (:report (:id "p" :kind note)))
            (branch-summary "s" (:branch-id "b" :summary "x" :metadata nil))
            (compaction "s" (:summary "x"))
            (provider-anchor "s" (:provider-id openai))
            (context-generation "s" (:generation ,generation))
            (context-curation-package "s"
                                      (:package (:promotion ,promotion :erasure nil)))
            (clear-messages "s" nil)
            (board-message "s" (:message (:id "board" :kind output)))
            (board-state "s" (:principal "p" :board-id "b"
                              :association-role nil :routing-policy nil))
            (board-messages-clear "s" nil)
            (delete "s" nil)
            (session-info "s" (:field name :value "name"))))
         (measure (lambda (body)
                    (e-runtime-store-codec-measure-bounded
                     body e-runtime-store-codec-protocol-canonical-byte-limit))))
    (should (equal (mapcar #'car cases) e-session-aggregate-command-tags))
    (dolist (case cases)
      (pcase-let ((`(,tag ,session-id ,arguments) case))
        (let* ((accounting (e-session-aggregate-command-accounting
                            tag session-id arguments measure))
               (command (e-session-aggregate-command-seal
                         tag session-id arguments accounting measure))
               (command-session-id
                (e-session-aggregate-command-session-id command))
               (delta (e-session-aggregate-command-interpret store command))
               (actual-body
                (if (eq tag 'delete)
                    (list :op 'session-delete :session-id command-session-id)
                  (list :op 'session-append
                        :session-id command-session-id
                        :record (plist-get delta :record)))))
          (should (= (plist-get accounting :frame-reserve)
                     (funcall measure
                              (e-session-aggregate-command-maximum-transport-body
                               tag arguments))))
          (should (<= (funcall measure actual-body)
                      (plist-get accounting :frame-reserve))))))))

(ert-deftest e-session-async-s92-board-and-curation-share-one-frozen-p ()
  "Command, delta, live state, and result share variable producer leaves."
  (let* ((store (e-session-store-create))
         (_session (e-session-aggregate-create store :id "identity"))
         (_generation
          (e-session-aggregate-append-context-generation
           store "identity"
           (e-context-lifetime-generation-create
            :id "generation" :checkpoint nil
            :covered-session-boundary
            (plist-get (e-session-get store "identity") :root-event-id))))
         (large (make-string 7000 ?x))
         (promotion
          (list :record-version 3 :type 'context-promotion :id "promotion"
                :frame-id "frame" :generation-id "generation"
                :consumer-request-id "consumer" :response-entry-id "response"
                :items
                (list (list :kind 'exact :value large
                            :source-observation-ids '("observation")
                            :source-refs '("source")
                            :source-fingerprints '("fingerprint")))))
         (package (list :promotion promotion :erasure nil))
         (command (e-session-async-test--seal
                   store 'context-curation-package "identity"
                   (list :package package)))
         (frozen-promotion
          (plist-get (plist-get (e-session-aggregate-command-arguments command)
                                :package)
                     :promotion))
         (delta (e-session-aggregate-command-interpret store command)))
    (should-not (eq frozen-promotion promotion))
    (should (eq (plist-get (plist-get delta :record) :promotion)
                frozen-promotion))
    (e-session-aggregate-apply-committed-record store (plist-get delta :record))
    (let* ((entry (e-session-aggregate-entry-by-id
                   store "identity" (plist-get delta :result-id)))
           (result (e-session-aggregate-command-result store command delta)))
      (should (eq (plist-get entry :promotion) frozen-promotion))
      (should (eq (plist-get result :promotion) frozen-promotion))
      (should-not (plist-member (plist-get result :entry) :durability-state)))
    (let* ((content (make-string 16384 ?b))
           (board-command
            (e-session-async-test--seal
             store 'board-message "identity"
             (list :message (list :id "board" :kind 'output :content content))))
           (frozen-message
            (plist-get (e-session-aggregate-command-arguments board-command)
                       :message))
           (board-delta
            (e-session-aggregate-command-interpret store board-command))
           (durable-message
            (plist-get (plist-get board-delta :record) :message)))
      ;; The interpreter owns a separately bounded normalized spine, but the
      ;; variable producer leaf is the one frozen allocation charged to P.
      (should-not (eq durable-message frozen-message))
      (should (eq (plist-get durable-message :content)
                  (plist-get frozen-message :content)))
      (e-session-aggregate-apply-committed-record
       store (plist-get board-delta :record))
      (let* ((journal (gethash "identity"
                               (e-session-store-board-journals store)))
             (live (car (e-session-board-journal-messages journal)))
             (result (e-session-aggregate-command-result
                      store board-command board-delta)))
        (should (eq live durable-message))
        (should-not (eq result live))
        (should (eq (plist-get result :content)
                    (plist-get frozen-message :content)))))))

(ert-deftest e-session-async-s92-noop-never-dirties-projection ()
  "Missing and duplicate semantic no-ops neither submit nor mark projection."
  (let* ((directory (make-temp-file "e-session-noop-projection-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (marks 0))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "noop"))
          (e-session-async-test--wait-finished
           (e-session-append-board-message
            store "noop" '(:id "same" :kind output :content "same")))
          (cl-letf (((symbol-function 'e-session-storage--mark-checkpoint-dirty)
                     (lambda (&rest _) (cl-incf marks))))
            (e-session-async-test--wait-finished
             (e-session-set-message-display store "noop" "missing" 'hidden))
            (e-session-async-test--wait-finished
             (e-session-append-board-message
              store "noop" '(:id "same" :kind output :content "same"))))
          (should (= marks 0))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-first-board-append-creates-journal-only-after-ack ()
  "Held or failed first Board transport never creates pre-ACK journal state."
  (let* ((directory (make-temp-file "e-session-board-held-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         callback work)
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "held-board"))
          (cl-letf (((symbol-function 'e-session-storage-submit)
                     (lambda (owner _kind _body settle &optional escrow)
                       (e-session-storage-release-frame-escrow owner escrow)
                       (setq callback settle)
                       t)))
            (setq work (e-session-append-board-message
                        store "held-board" '(:id "first" :kind output)))
            (let ((deadline (+ (float-time) 2.0)))
              (while (and (null callback) (< (float-time) deadline))
                (sit-for 0.01)))
            (should callback)
            (should-not (gethash "held-board"
                                 (e-session-store-board-journals store)))
            (funcall callback nil '(e-session-storage-error "failed"))
            (should (eq (plist-get (e-session-async-test--wait work) :state)
                        'failed)))
          (should-not (gethash "held-board"
                               (e-session-store-board-journals store)))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-malformed-new-families-fail-before-admission ()
  "Malformed Board, context, and process inputs cannot poison a valid tail."
  (let* ((directory (make-temp-file "e-session-invalid-command-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t)))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "invalid"))
          (dolist (work
                   (list
                    (e-session-append-board-message
                     store "invalid" '(:id "bad" :record-type unknown))
                    (e-session-append-context-curation-package
                     store "invalid"
                     '(:promotion (:record-version 2 :type context-promotion)
                       :erasure nil))
                    (e-session-append-process-report
                     store "invalid" '(not-a-plist))))
            (should (eq (plist-get (e-session-async-test--wait work) :state)
                        'failed))
            (e-session-async-test--assert-zero-ownership store))
          (should
           (equal (plist-get
                   (e-session-async-test--wait-finished
                    (e-session-append-process-report
                     store "invalid" '(:kind note :value "tail")))
                   :value)
                  "tail"))
          (dolist (too-long
                   (list
                    (e-session-declare-board-state
                     store "invalid" "p" (make-string 129 ?b))
                    (e-session-append-process-report
                     store "invalid" (list :id (make-string 129 ?p)))))
            (should (eq (plist-get (e-session-async-test--wait too-long) :state)
                        'failed)))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-session-info-invalid-subtypes-never-submit ()
  "Every state-independent session-info error precedes ownership and I/O."
  (let* ((directory (make-temp-file "e-session-invalid-info-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (long-id (make-string 129 ?x))
         before works)
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "invalid-info"))
          (setq before
                (length (plist-get (e-session-get store "invalid-info")
                                   :session-events)))
          (cl-letf (((symbol-function 'e-session-storage-submit)
                     (lambda (&rest _)
                       (ert-fail "invalid session-info reached storage I/O"))))
            (setq works
                  (list
                   (e-session-set-metadata
                    store "invalid-info" '(:unknown 1))
                   (e-session-set-session-config
                    store "invalid-info" '(:org-canvas-ref nil))
                   (e-session-set-context-references
                    store "invalid-info" long-id nil)
                   (e-session-set-context-reference
                    store "invalid-info" :unknown nil)
                   (e-session-set-capability-state
                    store "invalid-info" long-id nil)
                   (e-session-set-turn-options
                    store "invalid-info" '(:unknown 1))
                   (e-session-set-current-branch
                    store "invalid-info" long-id)
                   (e-session-rename store "invalid-info" " \t\n"))))
          (dolist (work works)
            (should (eq (plist-get (e-session-async-test--wait work) :state)
                        'failed))
            (e-session-async-test--assert-zero-ownership store))
          (should (= before
                     (length (plist-get (e-session-get store "invalid-info")
                                        :session-events))))
          (should
           (equal (plist-get
                   (e-session-async-test--wait-finished
                    (e-session-append-process-report
                     store "invalid-info" '(:kind note :value "tail")))
                   :value)
                  "tail"))
          (e-session-async-test--assert-zero-ownership store))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-board-categories-bound-before-intern ()
  "Repeated oversized Board categories settle without I/O or symbol leakage."
  (let* ((directory (make-temp-file "e-session-board-category-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (category (concat "uninterned-board-category-" (make-string 129 ?x)))
         works)
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "board-category"))
          (should-not (intern-soft category))
          (cl-letf (((symbol-function 'e-session-storage-submit)
                     (lambda (&rest _)
                       (ert-fail "oversized Board category reached storage"))))
            (dotimes (iteration 2)
              (dolist (message
                       (list (list :id (format "kind-%s" iteration)
                                   :kind category)
                             (list :id (format "tag-%s" iteration)
                                   :tags (list category))
                             (list :id (format "status-%s" iteration)
                                   :attributes (list :status category))))
                (push (e-session-append-board-message
                       store "board-category" message)
                      works))))
          (dolist (work works)
            (should (eq (plist-get (e-session-async-test--wait work) :state)
                        'failed))
            (e-session-async-test--assert-zero-ownership store))
          (should-not (intern-soft category)))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-s92-board-routing-retains-frozen-leaves ()
  "Board routing copies bounded spines while sharing its one frozen payload."
  (let* ((store (e-session-store-create))
         (_session (e-session-aggregate-create store :id "routing"))
         (large (make-string 60000 ?r))
         (policy
          (list :participant-id "participant"
                :pickup-selector
                (list :kind "input" :tags '(private)
                      :attributes (list :source large))
                :observer-selector '(:kind activity :tags-all (private))
                :default-tags '(private)
                :default-to "participant"))
         (arguments (list :principal "principal" :board-id "board"
                          :association-role nil :routing-policy policy))
         (command (e-session-async-test--seal
                   store 'board-state "routing" arguments))
         (frozen-policy
          (plist-get (e-session-aggregate-command-arguments command)
                     :routing-policy))
         (frozen-leaf
          (plist-get
           (plist-get (plist-get frozen-policy :pickup-selector) :attributes)
           :source))
         (delta (e-session-aggregate-command-interpret store command))
         (state (plist-get (plist-get delta :record) :board-state))
         (durable-policy (plist-get state :routing-policy))
         (measure
          (lambda (body)
            (e-runtime-store-codec-measure-bounded
             body e-runtime-store-codec-protocol-canonical-byte-limit))))
    (should-not (eq frozen-leaf large))
    (should (eq (plist-get
                 (plist-get (plist-get durable-policy :pickup-selector)
                            :attributes)
                 :source)
                frozen-leaf))
    (should (<= (funcall measure
                         (list :op 'session-append :session-id "routing"
                               :record (plist-get delta :record)))
                (plist-get (e-session-aggregate-command-account command)
                           :frame-reserve)))
    (e-session-aggregate-apply-committed-record store (plist-get delta :record))
    (let* ((live (plist-get (e-session-get store "routing")
                            :board-session-state))
           (result (e-session-aggregate-command-result store command delta)))
      (should (eq live state))
      (should-not (eq result live))
      (should (eq (plist-get
                   (plist-get
                    (plist-get (plist-get result :routing-policy)
                               :pickup-selector)
                    :attributes)
                   :source)
                  frozen-leaf)))))

(ert-deftest e-session-async-s92-board-routing-preflight-measures-virtual-wire ()
  "Routing admission measures exact tagged wire bytes without materializing."
  (let* ((make-policy
          (lambda (attributes)
            (list :participant-id "participant"
                  :pickup-selector
                  (list :kind "input" :tags '(private)
                        :attributes attributes)
                  :observer-selector '(:kind activity :tags-all (private))
                  :default-tags '(private)
                  :default-to "participant")))
         (representatives
          (list (funcall make-policy
                         (list :source
                               (concat "ascii/\\\"" (string 0 8 9 10 12 13 31))))
                (funcall make-policy
                         (list :source (concat "météo-中"
                                              (string #x2028 #x2029))
                               :mode 'compact))
                (funcall make-policy
                         (list :source (vector "vector" '(nested values))))
                (funcall make-policy
                         (list :source (unibyte-string 128 255)))
                (plist-put (funcall make-policy nil)
                           :observer-selector nil))))
    (dolist (policy representatives)
      (should
       (= (e-session-board-policy-wire-json-byte-size policy)
          (string-bytes
           (json-encode
            (e-session-codec-board-routing-policy-for-json policy))))))
    (dolist (scalar (list nil t json-false 0 -1 1.5 'category :category
                          "a/b\\c\"d" (string 0 8 9 10 12 13 31 32
                                                   #x2028 #x2029 #x00e9 #x4e2d)
                          (unibyte-string 128 255)))
      (should (= (e-session-board-policy--json-scalar-byte-size scalar)
                 (string-bytes (json-encode scalar)))))
    (let* ((empty-policy (funcall make-policy '(:source "")))
           (overhead (e-session-board-policy-wire-json-byte-size empty-policy))
           (payload-bytes (- e-session-board-policy--byte-budget overhead))
           (exact (funcall make-policy
                           (list :source (make-string payload-bytes ?x))))
           (one-over (funcall make-policy
                              (list :source
                                    (make-string (1+ payload-bytes) ?x)))))
      (should (= (e-session-board-policy-wire-json-byte-size exact)
                 e-session-board-policy--byte-budget))
      (should
       (= (string-bytes
           (json-encode
            (e-session-codec-board-routing-policy-for-json exact)))
          e-session-board-policy--byte-budget))
      (should (e-session-board-routing-policy-valid-p exact))
      (should (= (e-session-board-policy-wire-json-byte-size one-over)
                 (1+ e-session-board-policy--byte-budget)))
      (should-not (e-session-board-routing-policy-valid-p one-over)))
    (let ((policy (funcall make-policy '(:source "no-copy"))))
      (cl-letf (((symbol-function
                  'e-session-codec-board-routing-policy-for-json)
                 (lambda (&rest _)
                   (ert-fail "routing preflight materialized codec value")))
                ((symbol-function 'json-encode)
                 (lambda (&rest _)
                   (ert-fail "routing preflight encoded a scalar")))
                ((symbol-function 'json-serialize)
                 (lambda (&rest _)
                   (ert-fail "routing preflight serialized a scalar"))))
        (should
         (e-session-aggregate-command-accounting
          'board-state "routing-preflight"
          (list :principal "principal" :board-id "board"
                :association-role nil :routing-policy policy)
          (lambda (_body) 1)))))))

(provide 'e-session-async-test)

;;; e-session-async-test.el ends here
