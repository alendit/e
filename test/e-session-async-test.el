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
  (let* ((wrapper-nodes (pcase tag ('create 2) ('append-message 6) (_ 4)))
         ;; Hash + one entry + vector account for three further nodes.
         (tree-nodes (- node-count wrapper-nodes 3))
         (table (make-hash-table :test 'eq)))
    (when (< tree-nodes 0)
      (error "Node fixture is smaller than its command wrapper"))
    (puthash :nested
             (vector (e-session-async-test--balanced-cons-tree tree-nodes))
             table)
    (pcase tag
      ('create (list :metadata table))
      ('append-message (list :message (list :role 'user :content table)))
      ('session-info (list :field 'metadata :value table)))))

(ert-deftest e-session-async-s92-domain-accounting-schema-is-exact ()
  "One aggregate schema owns generated D/R and exact family frame escrow."
  (let* ((directory (make-temp-file "e-session-accounting-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (cases '((create (:metadata nil) 262144)
                  (append-message (:message (:role user :content "x")) 131072)
                  (session-info (:field name :value "name") 98304))))
    (unwind-protect
        (dolist (case cases)
          (pcase-let ((`(,tag ,arguments ,expected-d) case))
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
              (should (= (plist-get accounting :producer-max) 16777216))
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
          `((create "frame-create" (:metadata (:project-root ,maximum)))
            (append-message "frame-base"
                            (:message (:role user :content ,maximum)))
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
                   context-curation-response session-info))))

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

(provide 'e-session-async-test)

;;; e-session-async-test.el ends here
