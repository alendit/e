;;; e-session-test.el --- Tests for e sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for session storage.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-dev-profile)
(require 'e-session)
(require 'e-board)

(ert-deftest e-session-test-create-and-read ()
  "Sessions can be created and read by id."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1" :metadata '(:model "fake"))
    (should (equal (plist-get (e-session-get store "session-1") :id) "session-1"))
    (should (equal (e-session-messages store "session-1") nil))))

(ert-deftest e-session-test-board-log-deduplicates-and-clear-survives-replay ()
  "Board log identity and reset boundaries remain durable across reopen."
  (let ((directory (make-temp-file "e-session-board-log-" t)))
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (first '(:id "board-1" :kind output :content "old"))
               (second '(:id "board-2" :kind output :content "new")))
          (e-session-create store :id "board-session")
          (e-session-append-board-message store "board-session" first)
          (e-session-append-board-message store "board-session" first)
          (should (= (length (e-session-board-messages
                              store "board-session"))
                     1))
          (e-session-clear-board-messages store "board-session")
          (e-session-append-board-message store "board-session" second)
          (e-session-flush-write-queue store)
          (let ((reopened (e-session-persistent-store-create directory)))
            (let ((messages (e-session-board-messages
                             reopened "board-session")))
              (should (= (length messages) 1))
              (should (equal (plist-get (car messages) :id) "board-2"))
              (should (equal (plist-get (car messages) :content) "new")))
            (e-session-append-board-message reopened "board-session" second)
            (should (= (length (e-session-board-messages
                                reopened "board-session"))
                       1))))
      (delete-directory directory t))))

(ert-deftest e-session-test-board-log-rejects-divergent-typed-envelope-retries ()
  "A typed journal retry must exactly match the envelope it already appended."
  (let ((store (e-session-store-create))
        (first '(:id "chain" :record-type processing-chain
                 :root-message-id "root" :created-at "fixed"))
        (divergent '(:id "chain" :record-type processing-chain
                     :root-message-id "other" :created-at "fixed")))
    (e-session-create store :id "board-session")
    (e-session-append-board-message store "board-session" first)
    (e-session-append-board-message store "board-session" first)
    (should-error
     (e-session-append-board-message store "board-session" divergent)
     :type 'e-session-board-message-conflict)
    (should (equal (e-session-board-messages store "board-session")
                   (list first)))))

(ert-deftest e-session-test-processing-journal-rejects-cross-record-reentrancy-in-order ()
  "The live ledger and replay retain the same durable processing order."
  (let ((directory (make-temp-file "e-session-processing-order-" t))
        reentrant-error)
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (session-id "board-session"))
          (e-session-create store :id session-id)
          (let ((board
                 (e-board-create
                  :id "board"
                  :processing-record-notification-function
                  (lambda (callback-board record _type)
                    (e-session-append-board-message
                     store session-id
                     (e-board-processing-record-envelope record))
                    (condition-case error
                        (e-board-record-processing-chain
                         callback-board :id "nested" :root-message-id "root"
                         :candidate-message-id "nested" :caused-by-message-id "root"
                         :processor-history nil :processing-depth 0)
                      (e-board-id-conflict
                       (setq reentrant-error error)))))))
            (e-board-record-processing-chain
             board :id "outer" :root-message-id "root"
             :candidate-message-id "outer" :caused-by-message-id "root"
             :processor-history nil :processing-depth 0)
            (should reentrant-error)
            (should (equal (mapcar #'e-board-processing-chain-id
                                   (e-board-list-processing-chains board))
                           '("outer")))
            (let* ((reopened (e-session-persistent-store-create directory))
                   (restored (e-board-create :id "restored" :register nil)))
              (dolist (envelope (e-session-board-messages reopened session-id))
                (e-board-import-processing-record restored envelope))
              (should (equal (mapcar #'e-board-processing-chain-id
                                     (e-board-list-processing-chains restored))
                             '("outer"))))))
      (delete-directory directory t))))

(ert-deftest e-session-test-board-journal-is-private-from-generic-session-view ()
  "Generic session mutation cannot alter the private board journal or its index."
  (let ((store (e-session-store-create))
        (message '(:id "board-1" :kind output :content "retained")))
    (e-session-create store :id "board-session")
    (e-session-append-board-message store "board-session" message)
    (let ((session (e-session-get store "board-session")))
      (should-not (plist-member session :board-messages))
      (should-not (plist-member session :board-message-id-index))
      (plist-put session :board-messages
                 (list '(:id "board-1" :kind output :content "mutated")))
      (plist-put session :board-message-id-index (make-hash-table :test 'equal)))
    (e-session-append-board-message store "board-session" message)
    (should (equal (e-session-board-messages store "board-session")
                   (list message)))))

(ert-deftest e-session-test-board-log-rejects-cyclic-envelope-values ()
  "Board journals reject cyclic cons, vector, and hash table envelope values."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "board-session")
    (dolist (value
             (list (let ((cycle (list nil)))
                     (setcar cycle cycle)
                     cycle)
                   (let ((cycle (vector nil)))
                     (aset cycle 0 cycle)
                     cycle)
                   (let ((cycle (make-hash-table :test 'eq)))
                     (puthash :self cycle cycle)
                     cycle)))
      (should-error
       (e-session-append-board-message
        store "board-session" (list :id "cycle" :value value))
       :type 'e-session-board-message-cycle))
    (should-not (e-session-board-messages store "board-session"))))

(ert-deftest e-session-test-board-messages-loads-an-indexed-session ()
  "Board message access loads an unloaded indexed session before reading it."
  (let ((directory (make-temp-file "e-session-board-indexed-access-" t)))
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (session-id "board-session"))
          (e-session-create store :id session-id)
          (e-session-append-board-message
           store session-id '(:id "message-1" :kind output))
          (e-session-flush-write-queue store)
          (let ((indexed (e-session-persistent-index-store-create directory)))
            (should-not (plist-get (e-session--peek-session indexed session-id)
                                   :loaded))
            (should (equal (mapcar (lambda (message) (plist-get message :id))
                                   (e-session-board-messages indexed session-id))
                           '("message-1")))
            (should (plist-get (e-session--peek-session indexed session-id)
                               :loaded))))
      (delete-directory directory t))))

(ert-deftest e-session-test-board-log-freezes-input-and-returned-envelopes ()
  "Board journal state remains private across input and return-value mutation."
  (let* ((directory (make-temp-file "e-session-board-freeze-" t))
         (store (e-session-persistent-index-store-create directory :write-mode 'queued))
         (id (copy-sequence "frozen"))
         (value (copy-sequence "top-level"))
         (nested-value (copy-sequence "original"))
         (nested (list nested-value))
         (envelope (list :id id :value value :attributes (list :nested nested))))
    (unwind-protect
        (progn
          (e-session-create store :id "board-session")
          (let ((returned (e-session-append-board-message
                           store "board-session" envelope)))
            (aset id 0 ?x)
            (aset value 0 ?x)
            (aset nested-value 0 ?x)
            (aset (plist-get returned :id) 0 ?x)
            (aset (plist-get returned :value) 0 ?x)
            (aset (car (plist-get (plist-get returned :attributes) :nested))
                  0 ?x))
          (let ((message (car (e-session-board-messages store "board-session"))))
            (should (equal (plist-get message :id) "frozen"))
            (should (equal (plist-get message :value) "top-level"))
            (should (equal (plist-get (plist-get message :attributes) :nested)
                           '("original"))))
          (e-session-flush-write-queue store)
          (let ((message (car (e-session-board-messages
                               (e-session-persistent-store-create directory)
                               "board-session"))))
            (should (equal (plist-get message :id) "frozen"))
            (should (equal (plist-get message :value) "top-level"))
            (should (equal (plist-get (plist-get message :attributes) :nested)
                           '("original")))))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-test-checkpoint-manifest-deep-freezes-all-values ()
  "Manifest mutation cannot alter generic session state or retained entry IDs."
  (let* ((store (e-session-store-create))
         (session-id (copy-sequence "session-1"))
         (name (copy-sequence "session name"))
         (project-root (copy-sequence "project root"))
         (branch-id (copy-sequence "branch-1")))
    (e-session-create store :id session-id
                      :metadata (list :name name :project-root project-root))
    (e-session-append-message store session-id
                              (list :role 'user :content "message"))
    (e-session-set-current-branch store session-id branch-id)
    (let* ((manifest (e-session-checkpoint-manifest store session-id))
           (root (plist-get manifest :root))
           (entry-id (aref (plist-get manifest :entry-ids) 0)))
      (dolist (value (list (plist-get manifest :session-id)
                           (plist-get root :id)
                           (plist-get root :name)
                           (plist-get (plist-get root :metadata) :project-root)
                           (plist-get root :current-branch)
                           entry-id))
        (aset value 0 ?x)))
    (let ((session (e-session-get store session-id)))
      (should (equal session-id "session-1"))
      (should (equal (plist-get session :name) "session name"))
      (should (equal (plist-get (plist-get session :metadata) :project-root)
                     "project root"))
      (should (equal (plist-get session :current-branch) "branch-1"))
      (should (string-prefix-p "01" (plist-get session :root-event-id)))
      (should (string-prefix-p "01"
                               (plist-get (car (e-session-messages store session-id))
                                          :id))))))

(ert-deftest e-session-test-board-log-rejects-invalid-record-types ()
  "Only absent and supported processing record types enter the board journal."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "board-session")
    (e-session-append-board-message store "board-session" '(:id "message"))
    (dolist (record-type '("" 0 :json-false unknown "unknown"))
      (should-error
       (e-session-append-board-message
        store "board-session" (list :id "message" :record-type record-type))
       :type 'e-session-board-message-invalid-record-type))
    (should (equal (mapcar #'e-session--board-message-identity
                           (e-session-board-messages store "board-session"))
                   '((board-message . "message"))))))

(ert-deftest e-session-test-checkpoint-manifest-detaches-board-identities ()
  "Checkpoint manifest mutation cannot change the private board journal."
  (let* ((store (e-session-store-create))
         (session-id "board-session")
         (board-id (copy-sequence "board-1"))
         (principal (copy-sequence "principal-1"))
         (message-id (copy-sequence "record-1")))
    (e-session-create store :id session-id)
    (e-session-declare-board-state store session-id principal board-id)
    (e-session-append-board-message
     store session-id
     (list :id message-id :record-type 'processing-chain))
    (let* ((manifest (e-session-checkpoint-manifest store session-id))
           (identity (aref (plist-get manifest :board-message-identities) 0))
           (state (plist-get manifest :board-state)))
      (aset (plist-get identity :id) 0 ?x)
      (aset (plist-get state :board-id) 0 ?x)
      (aset (plist-get state :principal) 0 ?x))
    (should (equal (plist-get (car (e-session-board-messages store session-id)) :id)
                   "record-1"))
    (should (equal (plist-get (plist-get (e-session-get store session-id)
                                          :board-session-state)
                              :board-id)
                   "board-1"))
    (should (equal (plist-get (plist-get (e-session-get store session-id)
                                          :board-session-state)
                              :principal)
                   "principal-1"))))

(ert-deftest e-session-test-board-log-canonicalizes-processing-record-types ()
  "String and symbol processing record types share one durable identity."
  (let ((store (e-session-store-create))
        (symbol-envelope '(:id "record-1" :record-type processing-chain))
        (string-envelope '(:id "record-1" :record-type "processing-chain")))
    (e-session-create store :id "board-session")
    (e-session-append-board-message store "board-session" symbol-envelope)
    (e-session-append-board-message store "board-session" string-envelope)
    (let ((messages (e-session-board-messages store "board-session")))
      (should (= (length messages) 1))
      (should (eq (plist-get (car messages) :record-type)
                  'processing-chain)))))

(ert-deftest e-session-test-board-log-replay-deduplicates-identical-typed-envelope ()
  "Restart ignores repeated typed board envelopes with equal durable values."
  (let ((directory (make-temp-file "e-session-board-replay-duplicate-" t)))
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (session-id "board-session")
               (message '(:id "chain" :record-type processing-chain
                          :root-message-id "root" :created-at "fixed"))
               (record (list :type "board-message" :session-id session-id
                             :message message)))
          (e-session-create store :id session-id)
          (e-session-append-board-message store session-id message)
          (e-session--append-record-now store session-id record)
          (let ((messages (e-session-board-messages
                           (e-session-persistent-store-create directory) session-id)))
            (should (= (length messages) 1))
            (should (equal (plist-get (car messages) :id) "chain"))
            (should (eq (plist-get (car messages) :record-type)
                        'processing-chain))))
      (delete-directory directory t))))

(ert-deftest e-session-test-board-log-replay-rejects-divergent-typed-envelope ()
  "Restart rejects repeated typed board envelopes with divergent durable values."
  (let ((directory (make-temp-file "e-session-board-replay-conflict-" t)))
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (session-id "board-session")
               (message '(:id "chain" :record-type processing-chain
                          :root-message-id "root" :created-at "fixed"))
               (divergent '(:id "chain" :record-type processing-chain
                            :root-message-id "other" :created-at "fixed")))
          (e-session-create store :id session-id)
          (e-session-append-board-message store session-id message)
          (e-session--append-record-now
           store session-id
           (list :type "board-message" :session-id session-id
                 :message divergent))
          (should-error
           (e-session-board-messages
            (e-session-persistent-store-create directory) session-id)
           :type 'e-session-board-message-conflict))
      (delete-directory directory t))))

(ert-deftest e-session-test-board-log-keeps-colliding-record-kinds-across-restart ()
  "Board messages and processing records share raw ids without journal loss."
  (let ((directory (make-temp-file "e-session-board-collision-" t)))
    (unwind-protect
        (let ((store (e-session-persistent-store-create directory)))
          (e-session-create store :id "board-session")
          (dolist (envelope '((:id "shared" :kind output)
                              (:id "shared" :record-type processing-chain)
                              (:id "shared" :record-type processing-result)))
            (e-session-append-board-message store "board-session" envelope))
          (e-session-flush-write-queue store)
          (let ((reopened (e-session-persistent-store-create directory)))
            (should (equal
                     (mapcar #'e-session--board-message-identity
                             (e-session-board-messages reopened "board-session"))
                     '((board-message . "shared")
                       (processing-chain . "shared")
                       (processing-result . "shared"))))))
      (delete-directory directory t))))

(ert-deftest e-session-test-append-message-preserves-order ()
  "Messages are returned in insertion order."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-message store "session-1"
                              '(:id "msg-1" :role user :content "hello"))
    (e-session-append-message store "session-1"
                              '(:id "msg-2" :role assistant :content "hi"))
    (should (equal (mapcar (lambda (message) (plist-get message :id))
                           (e-session-messages store "session-1"))
                   '("msg-1" "msg-2")))))

(ert-deftest e-session-test-message-appends-maintain-derived-fields-incrementally ()
  "Message appends update metadata without full derived-field refreshes."
  (let ((store (e-session-store-create))
        (refresh-count 0)
        (original-refresh (symbol-function 'e-session--refresh-derived-fields)))
    (cl-letf (((symbol-function 'e-session--refresh-derived-fields)
               (lambda (refresh-store refresh-session)
                 (setq refresh-count (1+ refresh-count))
                 (funcall original-refresh refresh-store refresh-session))))
      (e-session-create store :id "session-1")
      (setq refresh-count 0)
      (e-session-append-message
       store "session-1"
       '(:id "msg-1" :role user :content "first"))
      (e-session-append-message
       store "session-1"
       '(:id "msg-2" :role assistant :content "second"))
      (let ((session (e-session-get store "session-1")))
        (should (= refresh-count 0))
        (should (= (plist-get session :message-count) 2))
        (should (equal (plist-get session :summary) "first"))
        (should (equal (plist-get session :last-message-at)
                       (plist-get (cadr (plist-get session :messages))
                                  :created-at)))))))

(ert-deftest e-session-test-append-message-tracks-latest-assistant-marker ()
  "Message appends maintain the latest assistant marker for unread checks."
  (let* ((store (e-session-store-create))
         (session-id "session-assistant-marker"))
    (e-session-create store :id session-id)
    (e-session-append-message
     store session-id
     '(:id "user-1" :role user :content "hello"))
    (should-not
     (plist-get (e-session-get store session-id) :latest-assistant-marker))
    (e-session-append-message
     store session-id
     '(:id "assistant-1" :role assistant :content "one"))
    (should (equal
             (plist-get (e-session-get store session-id)
                        :latest-assistant-marker)
             "assistant-1"))
    (e-session-append-message
     store session-id
     '(:id "assistant-2" :role assistant :content "two"))
    (should (equal
             (plist-get (car (e-session-list store))
                        :latest-assistant-marker)
              "assistant-2"))))

(ert-deftest e-session-test-append-assistant-allocates-board-output-sequence ()
  "New assistant entries receive a stable session-local board output sequence."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "output-sequence")
    (e-session-append-message store "output-sequence" '(:role user :content "one"))
    (let ((first (e-session-append-message
                  store "output-sequence" '(:role assistant :content "two")))
          (second (e-session-append-message
                   store "output-sequence" '(:role assistant :content "three"))))
      (should (= (plist-get first :board-output-sequence) 1))
      (should (= (plist-get second :board-output-sequence) 2)))))

(ert-deftest e-session-test-board-output-sequence-continues-after-replay ()
  "Reopened sessions allocate past stored assistant output identities."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "output-replay")
          (e-session-append-message
           store "output-replay" '(:role assistant :content "first"))
          (e-session-flush-write-queue store)
          (let* ((reopened (e-session-persistent-store-create directory))
                 (message (e-session-append-message
                           reopened "output-replay"
                           '(:role assistant :content "second"))))
            (should (= (plist-get message :board-output-sequence) 2))))
      (delete-directory directory t))))

(ert-deftest e-session-test-append-activity-allocates-board-activity-sequence ()
  "Durable activity entries receive one stable session-local publication sequence."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "activity-sequence")
    (let ((first (e-session-append-activity-event
                  store "activity-sequence" "turn" 'tool-started nil))
          (second (e-session-append-activity-event
                   store "activity-sequence" "turn" 'tool-finished nil)))
      (should (= (plist-get first :board-activity-sequence) 1))
      (should (= (plist-get second :board-activity-sequence) 2)))))

(ert-deftest e-session-test-board-activity-sequence-continues-after-replay ()
  "Reopened sessions retain their durable activity publication high watermark."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "activity-replay")
          (e-session-append-activity-event
           store "activity-replay" "turn" 'tool-started nil)
          (e-session-flush-write-queue store)
          (let* ((reopened (e-session-persistent-store-create directory))
                 (event (e-session-append-activity-event
                         reopened "activity-replay" "turn" 'tool-finished nil)))
            (should (= (plist-get event :board-activity-sequence) 2))))
      (delete-directory directory t))))

(ert-deftest e-session-test-append-message-stamps-created-at ()
  "Appended messages carry their creation timestamp."
  (let ((store (e-session-store-create)))
    (cl-letf (((symbol-function 'e-session--timestamp)
               (lambda (&optional _time) "2026-05-21T10:00:00Z")))
      (e-session-create store :id "session-1")
      (e-session-append-message
       store "session-1" '(:role user :content "hello"))
      (should (equal (plist-get (car (e-session-messages store "session-1"))
                                :created-at)
                     "2026-05-21T10:00:00Z")))))

(ert-deftest e-session-test-ulid-generation-is-ordered-and-opaque ()
  "Generated durable entry ids are ULID strings ordered by creation."
  (let ((ids nil))
    (cl-letf (((symbol-function 'float-time)
               (let ((times '(1770000000.001 1770000000.001 1770000000.002)))
                 (lambda (&optional _time)
                   (prog1 (car times)
                     (setq times (or (cdr times) times)))))))
      (setq ids (list (e-session-generate-ulid)
                      (e-session-generate-ulid)
                      (e-session-generate-ulid))))
    (dolist (id ids)
      (should (string-match-p "\\`[0-9A-HJKMNP-TV-Z]\\{26\\}\\'" id)))
    (should (equal ids (sort (copy-sequence ids) #'string<)))))

(ert-deftest e-session-test-generate-ulid-does-not-force-garbage-collect ()
  "Generated durable entry ids do not force global garbage collection."
  (cl-letf (((symbol-function 'garbage-collect)
             (lambda (&rest _args)
               (error "garbage-collect should not run"))))
    (dotimes (_ 5)
      (should (stringp (e-session-generate-ulid))))))

(ert-deftest e-session-test-append-message-assigns-entry-ids-and-parent-links ()
  "Appending messages assigns durable ids and links to the previous head."
  (let ((store (e-session-store-create)))
    (let* ((root (car (e-session-session-events
                       store
                       (plist-get (e-session-create store :id "session-1") :id))))
           (first (e-session-append-message
                   store "session-1" '(:role user :content "hello")))
           (second (e-session-append-message
                    store "session-1" '(:role assistant :content "hi")))
           (path (e-session-current-path store "session-1")))
      (should (string-match-p "\\`[0-9A-HJKMNP-TV-Z]\\{26\\}\\'"
                              (plist-get first :id)))
      (should (eq (plist-get root :event-type) 'session-created))
      (should-not (plist-get root :parent-id))
      (should (equal (plist-get first :parent-id)
                     (plist-get root :id)))
      (should (equal (plist-get second :parent-id)
                     (plist-get first :id)))
      (should (equal (mapcar (lambda (entry) (plist-get entry :id)) path)
                     (list (plist-get root :id)
                           (plist-get first :id)
                           (plist-get second :id)))))))


(ert-deftest e-session-test-current-path-uses-entry-index ()
  "Current-path traversal avoids repeated linear entry searches."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (dotimes (index 25)
      (e-session-append-message
       store
       "session-1"
       (list :role 'user :content (format "message-%d" index))))
    (let ((calls 0)
          (index (e-session--entry-index store "session-1")))
      (cl-letf (((symbol-function 'e-session--entries)
                 (lambda (&rest _args)
                   (setq calls (1+ calls))
                   nil)))
        (should (= (length (e-session-current-path store "session-1")) 26))
        (should (= calls 0))
        (clrhash index)
        (should-not (e-session-current-path store "session-1"))
        (should (> calls 0))))))

(ert-deftest e-session-test-missing-session-surfaces-error ()
  "Appending to a missing session surfaces a domain error."
  (let ((store (e-session-store-create)))
    (should-error
     (e-session-append-message store "missing" '(:role user :content "x"))
     :type 'e-session-missing)))

(ert-deftest e-session-test-set-message-display-updates-in-memory ()
  "Setting a message's display disposition flips its stored `:display'."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (let ((message (e-session-append-message
                    store "session-1"
                    '(:id "msg-1" :role assistant :content "hi"))))
      (e-session-set-message-display store "session-1"
                                     (plist-get message :id) 'hidden)
      (should (eq (plist-get (car (e-session-messages store "session-1"))
                             :display)
                  'hidden)))))

(ert-deftest e-session-test-set-message-display-survives-reload ()
  "A hidden-display update replays from disk so hiding is durable."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (e-session-append-message
           store "session-1" '(:id "msg-1" :role assistant :content "hi"))
          (e-session-set-message-display store "session-1" "msg-1" 'hidden)
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (eq (plist-get (car (e-session-messages loaded "session-1"))
                                   :display)
                        'hidden))))
      (delete-directory directory t))))

(ert-deftest e-session-test-message-origin-survives-reload-as-symbol ()
  "Persistent input provenance replays in the runtime's symbol form."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (e-session-append-message
           store "session-1"
           '(:id "msg-1" :role user :origin harness :content "repair"))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (message (car (e-session-messages loaded "session-1"))))
            (should (eq (plist-get message :origin) 'harness))))
      (delete-directory directory t))))

(ert-deftest e-session-test-persistent-session-generates-id-and-reloads ()
  "Persistent sessions get generated ids and replay messages in order."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session (e-session-create store))
         (session-id (plist-get session :id)))
    (unwind-protect
        (progn
          (should (string-match-p
                   "\\`[0-9]\\{8\\}T[0-9]\\{6\\}-[0-9a-f]\\{12\\}\\'"
                   session-id))
          (e-session-append-message
           store session-id '(:id "msg-1" :role user :content "hello"))
          (e-session-append-message
           store session-id '(:id "msg-2" :role assistant :content "hi"))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (mapcar (lambda (message) (plist-get message :id))
                           (e-session-messages loaded session-id))
                           '("msg-1" "msg-2")))))
      (delete-directory directory t))))

(ert-deftest e-session-test-persistent-appends-avoid-noisy-append-api ()
  "Persistent session appends avoid the API that emits write messages."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (append-to-file-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'append-to-file)
                   (lambda (start end filename)
                     (setq append-to-file-called t)
                     (write-region start end filename t 'silent))))
          (let* ((session (e-session-create store :id "session-quiet"))
                 (session-id (plist-get session :id)))
            (should-not append-to-file-called)
            (let ((append-to-file-called nil))
              (e-session-append-message
               store session-id
               '(:id "msg-1" :role user :content "quiet append"))
              (should-not append-to-file-called))
            (let* ((loaded (e-session-persistent-store-create directory))
                   (messages (e-session-messages loaded session-id)))
              (should (equal (plist-get (car messages) :content)
                             "quiet append")))))
      (delete-directory directory t))))

(ert-deftest e-session-test-queued-persistent-writes-flush-later ()
  "Queued persistent stores defer disk writes until the queue is flushed."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued))
         (write-count 0)
         (orig-write-region (symbol-function 'write-region)))
    (unwind-protect
        (cl-letf (((symbol-function 'write-region)
                   (lambda (&rest args)
                     (setq write-count (1+ write-count))
                     (apply orig-write-region args))))
          (let* ((session (e-session-create store :id "queued-session"))
                 (session-id (plist-get session :id)))
            (e-session-append-message
             store session-id
             '(:id "msg-1" :role user :content "queued hello"))
            (should (= write-count 0))
            (should (timerp (e-session-store-write-queue-timer store)))
            (should (= (length (e-session-store-write-queue store)) 2))
            (should (e-session-store-index-write-pending store))
            (e-session-flush-write-queue store)
            (should (> write-count 0))
            (should-not (e-session-store-write-queue store))
            (should-not (e-session-store-index-write-pending store))
            (let* ((loaded (e-session-persistent-store-create directory))
                   (messages (e-session-messages loaded session-id)))
              (should (equal (plist-get (car messages) :content)
                             "queued hello")))))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-test-queued-writes-carry-generation-metadata ()
  "Queued persistent record writes carry generation, sequence, and criticality."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued)))
    (unwind-protect
        (let* ((session (e-session-create store :id "queued-metadata"))
               (session-id (plist-get session :id)))
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "queued metadata"))
          (let ((entries (reverse (e-session-store-write-queue store))))
            (should (= (length entries) 2))
            (dolist (entry entries)
              (should (plist-member entry :session-id))
              (should (plist-member entry :record))
              (should (integerp (plist-get entry :generation)))
              (should (integerp (plist-get entry :sequence)))
              (should (eq (plist-get entry :criticality) 'critical))
              (should (plist-member entry :dependencies)))
            (should (< (plist-get (car entries) :sequence)
                       (plist-get (cadr entries) :sequence))))
          (let ((index-entry (e-session-store-index-write-pending store)))
            (should (plist-member index-entry :generation))
            (should (plist-member index-entry :sequence))
            (should (eq (plist-get index-entry :criticality) 'derived))))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-test-queued-record-criticality-covers-durable-record-types ()
  "Queued write criticality recognizes every persisted durable record type."
  (dolist (type '("session"
                  "session-info"
                  "message"
                  "activity-event"
                  "branch-summary"
                  "compaction"
                  "provider-anchor"
                  "process-report"
                  "context-generation"
                  "context-frame"
                  "context-promotion"
                  "context-frame-settlement"
                  "current-branch"
                  "messages-cleared"))
    (should (eq (e-session--queued-record-criticality
                 (list :type type))
                'critical)))
  (should (eq (e-session--queued-record-criticality
               '(:type "derived-preview"))
              'derived)))

(ert-deftest e-session-test-flush-write-queue-drops-stale-generation-records ()
  "Flushing a queued store ignores records from stale queue generations."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued)))
    (unwind-protect
        (let* ((session (e-session-create store :id "stale-queued"))
               (session-id (plist-get session :id)))
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "stale queued"))
          (cl-incf (e-session-store-write-queue-generation store))
          (e-session-flush-write-queue store)
          (let ((loaded (e-session-persistent-index-store-create directory)))
            (should-not
             (cl-find session-id (e-session-list loaded)
                      :key (lambda (entry) (plist-get entry :id))
                      :test #'equal))))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-test-flush-write-queue-recovers-stale-derived-index ()
  "Flushing current records rebuilds a stale derived index write."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued)))
    (unwind-protect
        (let* ((session (e-session-create store :id "recover-index"))
               (session-id (plist-get session :id))
               (stale-index (copy-sequence
                             (e-session-store-index-write-pending store))))
          (plist-put stale-index
                     :generation
                     (1- (e-session-store-write-queue-generation store)))
          (setf (e-session-store-index-write-pending store) stale-index)
          (e-session-flush-write-queue store)
          (let* ((loaded (e-session-persistent-index-store-create directory))
                 (entry (cl-find session-id
                                 (e-session-list loaded)
                                 :key (lambda (entry) (plist-get entry :id))
                                 :test #'equal)))
            (should entry)
            (should-not (plist-get entry :loaded))))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-test-flush-write-queue-orders-critical-before-derived-records ()
  "Queued flush writes critical records before non-critical derived records."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued))
         order)
    (unwind-protect
        (let* ((session-entry
                (e-session--queued-write-entry
                 store "ordered-critical" '(:type "session")))
               (derived-entry
                (e-session--queued-write-entry
                 store "ordered-critical" '(:type "derived-preview")))
               (message-entry
                (e-session--queued-write-entry
                 store "ordered-critical" '(:type "message"))))
          (setf (e-session-store-write-queue store)
                (list message-entry derived-entry session-entry))
          (e-session--adjust-unsettled-writes store 3)
          (cl-letf (((symbol-function 'e-session--append-record-now)
                     (lambda (_store _session-id record)
                       (push (plist-get record :type) order)))
                    ((symbol-function 'e-session--write-index-now)
                     (lambda (_store)
                       (push 'index order))))
            (e-session-flush-write-queue store))
          (should (equal (nreverse order)
                         '("session" "message" "derived-preview")))
          (should-not (e-session-store-write-queue store)))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-test-flush-write-queue-writes-critical-records-before-index ()
  "Queued flush writes critical session records before derived index state."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued))
         order)
    (unwind-protect
        (let* ((session (e-session-create store :id "critical-before-index"))
               (session-id (plist-get session :id)))
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "durable before index"))
          (cl-letf (((symbol-function 'e-session--append-record-now)
                     (lambda (_store _session-id record)
                       (push (plist-get record :type) order)))
                    ((symbol-function 'e-session--write-index-now)
                     (lambda (_store)
                       (push 'index order))))
            (e-session-flush-write-queue store))
          (should (equal (nreverse order)
                         '("session" "message" index)))
          (should-not (e-session-store-write-queue store))
          (should-not (e-session-store-index-write-pending store)))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-test-flush-write-queue-retries-only-unacknowledged-records ()
  "Queued flush preserves unacknowledged records after a partial write failure."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued))
         (message-failed nil)
         writes)
    (unwind-protect
        (let* ((session (e-session-create store :id "partial-flush"))
               (session-id (plist-get session :id)))
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "retry once"))
          (cl-letf (((symbol-function 'e-session--append-record-now)
                     (lambda (_store _session-id record)
                       (let ((type (plist-get record :type)))
                         (when (and (equal type "message")
                                    (not message-failed))
                           (setq message-failed t)
                           (error "simulated record write failure"))
                         (push type writes))))
                    ((symbol-function 'e-session--write-index-now)
                     (lambda (_store)
                       (push 'index writes))))
            (should-error (e-session-flush-write-queue store)
                          :type 'error)
            (should (equal (nreverse (copy-sequence writes))
                           '("session")))
            (should (= (length (e-session-store-write-queue store)) 1))
            (should (equal
                     (plist-get
                      (e-session--queued-entry-record
                       (car (e-session-store-write-queue store)))
                      :type)
                     "message"))
            (should (e-session-store-index-write-pending store))
            (e-session-flush-write-queue store)
            (should (equal (nreverse writes)
                           '("session" "message" index)))
            (should-not (e-session-store-write-queue store))
            (should-not (e-session-store-index-write-pending store))))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-test-flush-write-queue-settles-post-write-failure ()
  "A record written before its error is not appended a second time."
  (let* ((directory (make-temp-file "e-session-post-write-" t))
         (store (e-session-persistent-index-store-create
                 directory :write-mode 'queued))
         (original-append (symbol-function 'e-session--append-record-now))
         (failed nil))
    (unwind-protect
        (let* ((session (e-session-create store :id "post-write"))
               (session-id (plist-get session :id)))
          (e-session-append-board-message
           store session-id
           '(:id "chain" :record-type processing-chain :created-at "fixed"))
          (cl-letf (((symbol-function 'e-session--append-record-now)
                     (lambda (append-store append-session-id record)
                       (funcall original-append append-store append-session-id record)
                       (when (and (equal (plist-get record :type) "board-message")
                                  (not failed))
                         (setq failed t)
                         (error "simulated post-write failure")))))
            (e-session-flush-write-queue store))
          (should failed)
          (should-not (e-session-store-write-queue store))
          (let ((reopened (e-session-persistent-store-create directory)))
            (should (equal (mapcar #'e-session--board-message-identity
                                   (e-session-board-messages reopened session-id))
                           '((processing-chain . "chain"))))))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-test-flush-write-queue-retries-rebuilt-stale-index ()
  "A rebuilt stale derived index remains pending when index persistence fails."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-index-store-create
                 directory
                 :write-mode 'queued))
         (index-failed nil)
         writes)
    (unwind-protect
        (let* ((session (e-session-create store :id "retry-stale-index"))
               (session-id (plist-get session :id))
               (stale-index (copy-sequence
                             (e-session-store-index-write-pending store))))
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "retry index"))
          (plist-put stale-index
                     :generation
                     (1- (e-session-store-write-queue-generation store)))
          (setf (e-session-store-index-write-pending store) stale-index)
          (cl-letf (((symbol-function 'e-session--append-record-now)
                     (lambda (_store _session-id record)
                       (push (plist-get record :type) writes)))
                    ((symbol-function 'e-session--write-index-now)
                     (lambda (_store)
                       (if index-failed
                           (push 'index writes)
                         (setq index-failed t)
                         (error "simulated index write failure")))))
            (should-error (e-session-flush-write-queue store)
                          :type 'error)
            (should-not (e-session-store-write-queue store))
            (should (e-session-store-index-write-pending store))
            (should (e-session--queued-index-current-p
                     store
                     (e-session-store-index-write-pending store)))
            (e-session-flush-write-queue store)
            (should (equal (nreverse writes)
                           '("session" "message" index)))
            (should-not (e-session-store-write-queue store))
            (should-not (e-session-store-index-write-pending store))))
      (ignore-errors (e-session-flush-write-queue store))
      (delete-directory directory t))))

(ert-deftest e-session-test-load-session-start-replays-in-chunks ()
  "Chunked persistent session loading returns before replay completion."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session (e-session-create store :id "chunked-session"))
         (session-id (plist-get session :id)))
    (unwind-protect
        (progn
          (dotimes (index 8)
            (e-session-append-message
             store session-id
             (list :id (format "msg-%d" index)
                   :role 'user
                   :content (format "chunked message %d with enough bytes"
                                    index))))
          (let ((loaded (e-session-persistent-index-store-create directory))
                result
                failure
                progress
                request)
            (setq request
                  (e-session-load-session-start
                   loaded session-id
                   :chunk-bytes 256
                   :on-progress (lambda (payload)
                                  (push payload progress))
                   :on-done (lambda (session)
                              (setq result session))
                   :on-error (lambda (err)
                               (setq failure err))))
            (should (e-request-lifecycle-p request))
            (should (eq (e-request-lifecycle-state request) 'started))
            (should-not result)
            (let ((deadline (+ (float-time) 5)))
              (while (and (not result) (not failure) (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should-not failure)
            (should (eq (e-request-lifecycle-state request) 'finished))
            (should (plist-get result :loaded))
            (should (< 1 (length progress)))
            (should (equal (mapcar (lambda (message)
                                     (plist-get message :content))
                                   (e-session-messages loaded session-id))
                           (mapcar (lambda (index)
                                     (format
                                      "chunked message %d with enough bytes"
                                      index))
                                   (number-sequence 0 7))))))
      (delete-directory directory t))))

(ert-deftest e-session-test-load-requires-explicit-resume-checkpoint ()
  "Normal session load never falls back to a full checkpoint-less replay."
  (let* ((directory (make-temp-file "e-session-no-checkpoint-" t))
         (sessions-directory (expand-file-name "sessions" directory))
         (journal (expand-file-name "legacy.jsonl" sessions-directory)))
    (unwind-protect
        (progn
          (make-directory sessions-directory t)
          (with-temp-file journal
            (insert
             "{\"type\":\"session\",\"session-id\":\"legacy\",\"id\":\"root\",\"timestamp\":\"2026-08-10T00:00:00Z\"}\n"))
          (let ((store (e-session-persistent-index-store-create directory)))
            (should-error (e-session-load-session store "legacy")
                          :type 'e-session-checkpoint-missing)))
      (delete-directory directory t))))

(ert-deftest e-session-test-migrated-checkpoint-loads-only-journal-suffix ()
  "A migrated checkpoint restores state while journal I/O starts at its offset."
  (let* ((directory (make-temp-file "e-session-checkpoint-suffix-" t))
         (store (e-session-persistent-store-create directory))
         (session-id "session-1"))
    (unwind-protect
        (progn
          (e-session-create store :id session-id)
          (dotimes (index 20)
            (e-session-append-message
             store session-id
             (list :id (format "before-%d" index)
                   :role 'user :content (make-string 200 ?x))))
          (e-session-migrate-session-checkpoint store session-id)
          (let* ((checkpoint
                  (e-session--read-checkpoint store session-id))
                 (offset (plist-get checkpoint :journal-byte-offset)))
            (e-session-append-message
             store session-id '(:id "after" :role assistant :content "tail"))
            (let ((loaded (e-session-persistent-index-store-create directory))
                  (original (symbol-function 'insert-file-contents-literally))
                  starts)
              (cl-letf (((symbol-function 'insert-file-contents-literally)
                         (lambda (filename &optional visit beg end replace)
                           (when (string-suffix-p ".jsonl" filename)
                             (push (or beg 0) starts))
                           (funcall original filename visit beg end replace))))
                (should (= (length (e-session-messages loaded session-id)) 21)))
              (should starts)
              (should (cl-every (lambda (start) (>= start offset)) starts)))))
      (delete-directory directory t))))

(ert-deftest e-session-test-checkpoint-retains-model-path-and-bounds-audit-state ()
  "Resume state keeps compacted model context but bounds audit projections."
  (let* ((store (e-session-store-create))
         (session-id "checkpoint")
         (_ (e-session-create store :id session-id))
         (old (e-session-append-message
               store session-id '(:id "old" :role user :content "old")))
         (boundary (e-session-append-message
                    store session-id
                    '(:id "boundary" :role user :content "keep"))))
    (ignore old)
    (e-session-append-compaction
     store session-id "summary"
     :first-kept-entry-id (plist-get boundary :id))
    (let ((token-event
           (e-session-append-activity-event
            store session-id "turn" 'token-usage '(:input 10))))
      (dotimes (index 80)
        (e-session-append-activity-event
         store session-id "turn" 'tool-progress (list :index index)))
      (let ((answer (e-session-append-message
                     store session-id
                     '(:id "answer" :role assistant :content "new"))))
        (e-session-append-provider-anchor
         store session-id 'openai :model "model"
         :covered-entry-id (plist-get answer :id)
         :fingerprints '(:segments nil)))
      (e-session-append-board-message
       store session-id
       '(:id "durable-fact" :kind fact :tags (orchestration)))
      (dotimes (index 300)
        (e-session-append-board-message
         store session-id
         (list :id (format "board-%d" index)
               :kind 'activity :content index)))
      (let* ((records (e-session--checkpoint-records store session-id))
             (message-ids
              (mapcar (lambda (record)
                        (plist-get (plist-get record :message) :id))
                      (seq-filter
                       (lambda (record)
                         (equal (plist-get record :type) "message"))
                       records)))
             (activity-records
              (seq-filter
               (lambda (record)
                 (equal (plist-get record :type) "activity-event"))
               records)))
        (should (equal message-ids '("boundary" "answer")))
        (should-not (member "old" message-ids))
        (should (= (length activity-records) 65))
        (should (seq-find
                 (lambda (record)
                   (equal (plist-get record :id)
                          (plist-get token-event :id)))
                 activity-records))
        (let ((board-records
               (seq-filter
                (lambda (record)
                  (equal (plist-get record :type) "board-message"))
                records)))
          (should (= (length board-records) 257))
          (should (seq-find
                   (lambda (record)
                     (equal (plist-get (plist-get record :message) :id)
                            "durable-fact"))
                   board-records)))
        (should (= (length
                    (seq-filter
                     (lambda (record)
                       (equal (plist-get record :type) "provider-anchor"))
                     records))
                   1))))))

(ert-deftest e-session-test-persistent-replay-preserves-entry-ids ()
  "Persistent replay keeps durable ids and parent links instead of regenerating."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (let* ((first (e-session-append-message
                       store session-id '(:role user :content "hello")))
               (second (e-session-append-message
                        store session-id '(:role assistant :content "hi")))
               (loaded (e-session-persistent-store-create directory))
               (messages (e-session-messages loaded session-id)))
          (should (equal (mapcar (lambda (message) (plist-get message :id))
                                 messages)
                         (list (plist-get first :id)
                               (plist-get second :id))))
          (should (equal (plist-get (cadr messages) :parent-id)
                         (plist-get first :id))))
      (delete-directory directory t))))

(ert-deftest e-session-test-legacy-replay-backfills-entry-ids ()
  "Legacy records without entry ids load with stable in-memory parent links."
  (let* ((directory (make-temp-file "e-session-" t))
         (sessions-dir (expand-file-name "sessions" directory))
         (session-file (expand-file-name "legacy.jsonl" sessions-dir)))
    (unwind-protect
        (progn
          (make-directory sessions-dir t)
          (with-temp-file session-file
            (insert
             "{\"type\":\"session\",\"session-id\":\"legacy\",\"timestamp\":\"2026-05-21T10:00:00Z\"}\n"
             "{\"type\":\"message\",\"session-id\":\"legacy\",\"timestamp\":\"2026-05-21T10:00:01Z\",\"message\":{\"role\":\"user\",\"content\":\"hello\"}}\n"
             "{\"type\":\"message\",\"session-id\":\"legacy\",\"timestamp\":\"2026-05-21T10:00:02Z\",\"message\":{\"role\":\"assistant\",\"content\":\"hi\"}}\n"))
          (let ((migration-store
                 (e-session-persistent-index-store-create directory)))
            (e-session-migrate-session-checkpoint migration-store "legacy"))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (events (e-session-session-events loaded "legacy"))
                 (root (car events))
                 (messages (e-session-messages loaded "legacy")))
            (should (= (length events) 1))
            (should (eq (plist-get root :event-type) 'session-created))
            (should (plist-get root :id))
            (should (= (length messages) 2))
            (dolist (message messages)
              (should (plist-get message :id)))
            (should (equal (plist-get (car messages) :parent-id)
                           (plist-get root :id)))
            (should (equal (plist-get (cadr messages) :parent-id)
                           (plist-get (car messages) :id)))))
      (delete-directory directory t))))

(ert-deftest e-session-test-persistent-replay-refreshes-derived-fields-once-per-session ()
  "Persistent replay avoids per-record derived-field refresh work."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (dotimes (index 40)
            (e-session-append-message
             store
             session-id
             (list :id (format "msg-%d" index)
                   :role (if (cl-evenp index) 'user 'tool)
                   :content (list :payload (make-string 1000 ?x)))))
          (let ((refresh-count 0)
                (original-refresh
                 (symbol-function 'e-session--refresh-derived-fields)))
            (cl-letf (((symbol-function 'e-session--refresh-derived-fields)
                       (lambda (refresh-store refresh-session)
                         (setq refresh-count (1+ refresh-count))
                         (funcall original-refresh
                                  refresh-store
                                  refresh-session))))
              (let ((loaded (e-session-persistent-store-create directory)))
                (should (= (length (e-session-messages loaded session-id)) 40))
                (should (= refresh-count 1))))))
      (delete-directory directory t))))

(ert-deftest e-session-test-index-store-lists-without-loading-transcripts ()
  "Index-backed persistent stores list sessions before transcript replay."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-append-message
           store session-id '(:id "msg-1" :role user :content "indexed hello"))
          (e-session-append-message
           store session-id '(:id "msg-2" :role tool :content (:payload "large")))
          (should (string-prefix-p
                   "["
                   (with-temp-buffer
                     (insert-file-contents
                      (expand-file-name "index.json" directory))
                     (buffer-string))))
          (let ((original-insert (symbol-function 'insert-file-contents)))
            (cl-letf (((symbol-function 'insert-file-contents)
                       (lambda (filename &rest args)
                         (when (string-suffix-p ".jsonl" filename)
                           (error "index store loaded transcript"))
                         (apply original-insert filename args))))
              (let* ((indexed (e-session-persistent-index-store-create directory))
                     (sessions (e-session-list indexed))
                     (session (car sessions)))
                (should (equal (plist-get session :id) session-id))
                (should (equal (plist-get session :summary) "indexed hello"))
                (should (= (plist-get session :message-count) 2))
                (should-not (plist-get session :loaded)))))
          (let ((indexed (e-session-persistent-index-store-create directory)))
            (should (equal (mapcar (lambda (message) (plist-get message :id))
                                   (e-session-messages indexed session-id))
                           '("msg-1" "msg-2")))))
      (delete-directory directory t))))

(ert-deftest e-session-test-index-store-display-title-avoids-transcript-load ()
  "Display titles for indexed sessions use metadata without transcript replay."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get
                      (e-session-create store
                                        :id "session-1"
                                        :metadata '(:name "Indexed title"))
                      :id)))
    (unwind-protect
        (progn
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "indexed hello"))
          (let ((loaded nil))
            (cl-letf (((symbol-function 'e-session-load-session)
                       (lambda (&rest _args)
                         (setq loaded t)
                         (error "display title loaded transcript"))))
              (let ((indexed (e-session-persistent-index-store-create directory)))
                (should (equal (e-session-display-title indexed session-id)
                               "Indexed title"))
                (should-not loaded)))))
      (delete-directory directory t))))

(ert-deftest e-session-test-index-store-loads-object-shaped-index ()
  "Old object-shaped indexes still provide useful session metadata."
  (let ((directory (make-temp-file "e-session-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sessions" directory) t)
          (with-temp-file (expand-file-name "index.json" directory)
            (insert
             "{"
             "\"session-1\":{"
             "\"created-at\":\"2026-05-24T17:20:37Z\","
             "\"updated-at\":\"2026-05-24T17:21:00Z\","
             "\"summary\":\"object index prompt\","
             "\"title\":\"object index prompt\","
             "\"message-count\":3,"
             "\"last-message-at\":\"2026-05-24T17:21:00Z\""
             "}"
             "}\n"))
          (let* ((store (e-session-persistent-index-store-create directory))
                 (sessions (e-session-list store))
                 (session (car sessions)))
            (should (= (length sessions) 1))
            (should (equal (plist-get session :id) "session-1"))
            (should (equal (plist-get session :title)
                           "object index prompt"))
            (should (equal (plist-get session :summary)
                           "object index prompt"))
            (should (= (plist-get session :message-count) 3))
            (should-not (plist-get session :loaded))))
      (delete-directory directory t))))

(ert-deftest e-session-test-persistent-replay-preserves-message-timestamp ()
  "Persistent replay restores each message's journal timestamp."
  (let* ((directory (make-temp-file "e-session-" t))
         (timestamps '("2026-05-21T10:00:00Z"
                       "2026-05-21T10:00:01Z"))
         (store (cl-letf (((symbol-function 'e-session--timestamp)
                           (lambda (&optional _time)
                             (prog1 (car timestamps)
                               (setq timestamps (cdr timestamps))))))
                  (e-session-persistent-store-create directory)))
         (session-id nil))
    (unwind-protect
        (progn
          (setq timestamps '("2026-05-21T10:00:00Z"
                             "2026-05-21T10:00:01Z"))
          (cl-letf (((symbol-function 'e-session--timestamp)
                     (lambda (&optional _time)
                       (prog1 (car timestamps)
                         (setq timestamps (cdr timestamps))))))
            (setq session-id
                  (plist-get (e-session-create store :id "session-1") :id))
            (e-session-append-message
             store session-id '(:id "msg-1" :role user :content "hello")))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (plist-get (car (e-session-messages loaded session-id))
                                      :created-at)
                           "2026-05-21T10:00:01Z"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-rename-persists-explicit-title ()
  "Renaming a persistent session appends metadata and survives reload."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store) :id)))
    (unwind-protect
        (progn
          (e-session-rename store session-id "Named session")
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (e-session-display-title loaded session-id)
                           "Named session"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-default-title-uses-first-25-prompt-chars ()
  "Default session titles use only the first prompt snippet."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "short")
    (e-session-append-message
     store "short" '(:id "msg-1" :role user :content "abcdefghijklmnopqrstuvwxy"))
    (should (equal (e-session-display-title store "short")
                   "abcdefghijklmnopqrstuvwxy"))
    (e-session-create store :id "long")
    (e-session-append-message
     store "long" '(:id "msg-2" :role user :content "abcdefghijklmnopqrstuvwxyz"))
    (should (equal (e-session-display-title store "long")
                   "abcdefghijklmnopqrstuvwxy..."))))


(ert-deftest e-session-test-metadata-update-persists-through-session-info ()
  "Session metadata updates append session-info records and replay."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create
                                 store
                                 :id "session-1"
                                 :metadata '(:project-root "/tmp/narrow/"))
                                :id)))
    (unwind-protect
        (progn
          (e-session-set-metadata store session-id '(:project-root "/tmp/wide/"))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (plist-get
                            (plist-get (e-session-get loaded session-id) :metadata)
                            :project-root)
	                           "/tmp/wide/"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-replay-repairs-legacy-array-metadata ()
  "Persistent replay repairs legacy metadata arrays without relaxing writes."
  (let* ((directory (make-temp-file "e-session-legacy-array-metadata-" t))
         (sessions-directory (expand-file-name "sessions" directory))
         (session-file (expand-file-name "legacy-array.jsonl"
                                         sessions-directory)))
    (unwind-protect
        (progn
          (make-directory sessions-directory t)
          (with-temp-file session-file
            (insert
             (json-encode
              `(:type "session"
                :session-id "legacy-array"
                :id "root"
                :timestamp "2026-06-29T00:00:00Z"
                :metadata ["/tmp/project-a/"
                           "project-root"
                           "chat-default"
                           "harness-instance-id"]))
             "\n"
             (json-encode
              `(:type "session-info"
                :session-id "legacy-array"
                :id "info-1"
                :parent-id "root"
                :timestamp "2026-06-29T00:00:01Z"
                :metadata ["/tmp/project-b/"
                           "project-root"
                           "chat-updated"
                           "harness-instance-id"]))
             "\n"))
          (let ((migration-store
                 (e-session-persistent-index-store-create directory)))
            (e-session-migrate-session-checkpoint
             migration-store "legacy-array"))
          (let* ((store (e-session-persistent-store-create directory))
                 (metadata (plist-get (e-session-get store "legacy-array")
                                      :metadata)))
            (should (e-session--keyword-plist-shape-p metadata))
            (should (equal (plist-get metadata :project-root)
                           "/tmp/project-b/"))
            (should (equal (plist-get metadata :harness-instance-id)
                           "chat-updated"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-metadata-schema-rejects-transient-and-unknown-keys ()
  "Generic metadata writes reject unowned or presentation-only state."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (should-error
     (e-session-create
      store
      :id "legacy-array"
      :metadata '("project-root" "/tmp/project/")))
    (should-error
     (e-session-set-metadata store "session-1" '(:unknown t)))
    (should-error
     (e-session-set-metadata
      store "session-1" '(:e-chat-read-markers (:default "marker"))))
    (should-error
     (e-session-set-metadata
      store
      "session-1"
      '(:org-canvas (:uri "buffer://canvas"
                    :last-focus (:point 1)))))))

(ert-deftest e-session-test-typed-state-lanes-persist-through-replay ()
  "Typed metadata helpers persist their owned state lanes."
  (let* ((directory (make-temp-file "e-session-typed-state-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-set-session-config
           store session-id '(:project-root "/tmp/project/"))
          (e-session-set-context-references
           store
           session-id
           'chat-session
           '(:attachments ((:uri "buffer://source" :id "source"))))
          (e-session-set-capability-state
           store
           session-id
           'mcp
           '(:enabled t))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (metadata (plist-get
                            (e-session-get loaded session-id)
                            :metadata)))
            (should (equal (plist-get metadata :project-root)
                           "/tmp/project/"))
            (should (equal
                     (plist-get
                      (car (plist-get
                            (e-session-context-references
                             loaded session-id 'chat-session)
                            :attachments))
                      :uri)
                     "buffer://source"))
            (should (equal (plist-get
                            (e-session-capability-state
                             loaded session-id 'mcp)
                            :enabled)
                           t))))
      (delete-directory directory t))))

(ert-deftest e-session-test-subagent-lineage-metadata-round-trips ()
  "Durable subagent lineage metadata persists through replay."
  (let* ((directory (make-temp-file "e-session-subagent-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "child-1") :id)))
    (unwind-protect
        (progn
          (e-session-set-session-config
           store session-id
           '(:parent-session-id "parent-1"
             :subagent-role "reviewer"
             :subagent-label "review plan.org"
             :tmp-lineage-id "parent-1"))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (metadata (plist-get (e-session-get loaded session-id)
                                      :metadata)))
            (should (equal (plist-get metadata :parent-session-id) "parent-1"))
            (should (equal (plist-get metadata :subagent-role) "reviewer"))
            (should (equal (plist-get metadata :subagent-label)
                           "review plan.org"))
            (should (equal (plist-get metadata :tmp-lineage-id) "parent-1"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-replay-drops-known-transient-metadata ()
  "Legacy replay removes known presentation and high-churn focus metadata."
  (let* ((directory (make-temp-file "e-session-legacy-metadata-" t))
         (sessions-directory (expand-file-name "sessions" directory))
         (session-file (expand-file-name "legacy.jsonl" sessions-directory)))
    (unwind-protect
        (progn
          (make-directory sessions-directory t)
          (with-temp-file session-file
            (insert
             (json-encode
              '(:type "session"
                :session-id "legacy"
                :id "root"
                :timestamp "2026-06-29T00:00:00Z"
                :metadata (:name "Legacy"
                           :e-chat-read-markers (:default "marker")
                           :org-canvas (:uri "buffer://canvas"
                                        :last-scope "document"
                                        :last-focus (:point 42)))))
             "\n"))
          (let ((migration-store
                 (e-session-persistent-index-store-create directory)))
            (e-session-migrate-session-checkpoint migration-store "legacy"))
          (let* ((store (e-session-persistent-store-create directory))
                 (metadata (plist-get (e-session-get store "legacy")
                                      :metadata))
                 (canvas (plist-get metadata :org-canvas)))
            (should (equal (plist-get metadata :name) "Legacy"))
            (should-not (plist-member metadata :e-chat-read-markers))
            (should (equal (plist-get canvas :uri) "buffer://canvas"))
            (should-not (plist-member canvas :last-scope))
            (should-not (plist-member canvas :last-focus))))
      (delete-directory directory t))))

(ert-deftest e-session-test-turn-options-persist-through-session-info ()
  "Session turn options survive persistent replay."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store) :id)))
    (unwind-protect
        (progn
          (e-session-set-turn-options
           store
           session-id
           '(:model "gpt-test"
             :reasoning-effort "high"
             :prompt-cache-default t
             :prompt-cache-retention "24h"))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (e-session-turn-options loaded session-id)
                           '(:model "gpt-test"
                             :reasoning-effort "high"
                             :prompt-cache-default t
                             :prompt-cache-retention "24h")))))
      (delete-directory directory t))))

(ert-deftest e-session-test-branch-summary-persists-through-replay ()
  "Branch summary records append and replay into session state."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-append-branch-summary
           store session-id "branch-a" "Built the first slice."
           :metadata '(:from "turn-1"))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (summary (car (plist-get
                                (e-session-get loaded session-id)
                                :branch-summaries))))
            (should (equal (plist-get summary :branch-id) "branch-a"))
            (should (equal (plist-get summary :summary)
                           "Built the first slice."))
            (should (equal (plist-get summary :metadata)
                           '(:from "turn-1")))))
      (delete-directory directory t))))

(ert-deftest e-session-test-branch-summaries-preserve-append-order ()
  "Branch summaries stay in insertion order across multiple appends."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-branch-summary store "session-1" "branch-a" "First")
    (e-session-append-branch-summary store "session-1" "branch-b" "Second")
    (should (equal (mapcar (lambda (summary)
                             (plist-get summary :branch-id))
                           (plist-get (e-session-get store "session-1")
                                      :branch-summaries))
                   '("branch-a" "branch-b")))))

(ert-deftest e-session-test-compaction-persists-through-replay ()
  "Compaction records append and replay into session state."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-append-compaction
           store session-id "Compacted early transcript."
           :branch-id "branch-a"
           :range '(:from "msg-1" :to "msg-9")
           :tokens-before 123
           :tokens-kept 45)
          (let* ((loaded (e-session-persistent-store-create directory))
                 (compaction (car (plist-get
                                   (e-session-get loaded session-id)
                                   :compactions))))
            (should (equal (plist-get compaction :summary)
                           "Compacted early transcript."))
            (should (equal (plist-get compaction :branch-id) "branch-a"))
            (should (equal (plist-get compaction :range)
                           '(:from "msg-1" :to "msg-9")))
            (should (= (plist-get compaction :tokens-before) 123))
            (should (= (plist-get compaction :tokens-kept) 45))))
      (delete-directory directory t))))

(ert-deftest e-session-test-compactions-preserve-append-order ()
  "Compactions stay in insertion order across multiple appends."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-compaction store "session-1" "First")
    (e-session-append-compaction store "session-1" "Second")
    (should (equal (mapcar (lambda (compaction)
                             (plist-get compaction :summary))
                           (e-session-compactions store "session-1"))
                   '("First" "Second")))))

(ert-deftest e-session-test-provider-anchor-persists-through-replay ()
  "Provider anchors append and replay as opaque durable session entries."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (let* ((message (e-session-append-message
                         store session-id
                         '(:role assistant :content "anchored")))
               (anchor (e-session-append-provider-anchor
                        store session-id 'openai
                        :model "gpt-test"
                        :covered-entry-id (plist-get message :id)
                        :fingerprints '(:static-prefix "abc"
                                        :current-state "def")
                        :metadata '(:response-id "resp-1")))
               (loaded (e-session-persistent-store-create directory))
               (replayed (car (e-session-provider-anchors
                               loaded session-id))))
          (should (equal (plist-get replayed :id)
                         (plist-get anchor :id)))
          (should (eq (plist-get replayed :provider-id) 'openai))
          (should (equal (plist-get replayed :model) "gpt-test"))
          (should (equal (plist-get replayed :covered-entry-id)
                         (plist-get message :id)))
          (should (equal (plist-get replayed :fingerprints)
                         '(:static-prefix "abc"
                           :current-state "def")))
          (should (equal (plist-get replayed :metadata)
                         '(:response-id "resp-1"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-provider-anchor-persists-nested-fingerprints ()
  "Provider-anchor fingerprint arrays of plists survive JSONL replay."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id))
         (fingerprints
          '(:segments ((:kind "static-prefix"
                        :id "(project-local instructions)"
                        :fingerprint "static-fp")
                       (:kind "current-state"
                        :id "(visible-buffers 0)"
                        :fingerprint "dynamic-fp"))
            :active-layer-ids ("base" "project-local")
            :tools ((:name "read" :fingerprint "read-fp")
                    (:name "write" :fingerprint "write-fp"))
            :reasoning (:reasoning nil
                        :reasoning-effort "high"
                        :effort nil)
            :provider-options (:prompt-cache-key "cache-key"
                               :prompt-cache-retention "24h")
            :compaction-boundary nil)))
    (unwind-protect
        (let* ((message (e-session-append-message
                         store session-id
                         '(:role assistant :content "anchored")))
               (anchor (e-session-append-provider-anchor
                        store session-id 'openai
                        :model "gpt-test"
                        :covered-entry-id (plist-get message :id)
                        :fingerprints fingerprints
                        :metadata '(:response-id "resp-1")))
               (loaded (e-session-persistent-store-create directory))
               (replayed (car (e-session-provider-anchors
                               loaded session-id))))
          (should (equal (plist-get replayed :id)
                         (plist-get anchor :id)))
          (should (equal (plist-get replayed :fingerprints)
                         fingerprints)))
      (delete-directory directory t))))

(ert-deftest e-session-test-provider-anchor-rejects-malformed-replayed-segments ()
  "Malformed pre-fix provider-anchor segments invalidate without crashing."
  (let ((store (e-session-store-create)))
    (let* ((session-id (plist-get (e-session-create store :id "session-1") :id))
           (message (e-session-append-message
                     store session-id
                     '(:role assistant :content "anchored")))
           (malformed
            '(:segments (:kind ("static-prefix"
                                "id"
                                "(project-local instructions)"
                                "fingerprint"
                                "static-fp"))
              :active-layer-ids ("base" "project-local")
              :tools (:name ("read" "fingerprint" "read-fp"))
              :reasoning (:reasoning nil
                          :reasoning-effort "high"
                          :effort nil)
              :provider-options (:prompt-cache-key "cache-key")
              :compaction-boundary nil))
           (current
            '(:segments ((:kind "static-prefix"
                          :id "(project-local instructions)"
                          :fingerprint "static-fp"))
              :active-layer-ids ("base" "project-local")
              :tools ((:name "read" :fingerprint "read-fp"))
              :reasoning (:reasoning nil
                          :reasoning-effort "high"
                          :effort nil)
              :provider-options (:prompt-cache-key "cache-key")
              :compaction-boundary nil)))
      (e-session-append-provider-anchor
       store session-id 'openai
       :model "gpt-test"
       :covered-entry-id (plist-get message :id)
       :fingerprints malformed
       :metadata '(:response-id "resp-1"))
      (should (eq (e-session-provider-anchor-incompatibility-reason
                   store session-id
                   (car (e-session-provider-anchors store session-id))
                   'openai
                   "gpt-test"
                   current)
                  'segment-fingerprint-mismatch)))))

(ert-deftest e-session-test-latest-provider-anchor-requires-current-path ()
  "Provider anchors are compatible only when their covered entry is current."
  (let ((store (e-session-store-create)))
    (let* ((session-id (plist-get (e-session-create store :id "session-1") :id))
           (first (e-session-append-message
                   store session-id '(:role assistant :content "first")))
           (second (e-session-append-message
                    store session-id '(:role assistant :content "second")))
           (first-anchor
            (e-session-append-provider-anchor
             store session-id 'openai
             :model "gpt-test"
             :covered-entry-id (plist-get first :id)
             :fingerprints '(:history "one")
             :metadata '(:response-id "resp-1")))
           (second-anchor
            (e-session-append-provider-anchor
             store session-id 'openai
             :model "gpt-test"
             :covered-entry-id (plist-get second :id)
             :fingerprints '(:history "two")
             :metadata '(:response-id "resp-2"))))
      (should (equal (plist-get
                      (e-session-latest-compatible-provider-anchor
                       store session-id 'openai
                       :model "gpt-test"
                       :fingerprints '(:history "two"))
                      :id)
                     (plist-get second-anchor :id)))
      (should-not
       (e-session-latest-compatible-provider-anchor
        store session-id 'openai
        :model "gpt-test"
        :fingerprints '(:history "changed")))
      (plist-put (e-session-get store session-id)
                 :current-head-id
                 (plist-get first-anchor :id))
      (should (equal (plist-get
                      (e-session-latest-compatible-provider-anchor
                       store session-id 'openai
                       :model "gpt-test"
                       :fingerprints '(:history "one"))
                      :id)
                     (plist-get first-anchor :id)))
      (should-not
       (e-session-latest-compatible-provider-anchor
        store session-id 'openai
        :model "gpt-test"
        :fingerprints '(:history "two"))))))

(ert-deftest e-session-test-entry-query-helpers-cover-paths-turns-and-boundaries ()
  "Entry query helpers return ids, current paths, turn groups, and suffixes."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (let* ((first (e-session-append-message
                   store "session-1"
                   '(:turn-id "turn-a" :role user :content "one")))
           (second (e-session-append-message
                    store "session-1"
                    '(:turn-id "turn-a" :role assistant :content "two")))
           (third (e-session-append-message
                   store "session-1"
                   '(:turn-id "turn-b" :role user :content "three")))
           (compaction (e-session-append-compaction
                        store "session-1" "kept suffix"
                        :first-kept-entry-id (plist-get second :id))))
      (should (equal (plist-get (e-session-entry-by-id
                                 store "session-1" (plist-get second :id))
                                :content)
                     "two"))
      (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                             (e-session-entries-in-turn
                              store "session-1" "turn-a"))
                     (list (plist-get first :id)
                           (plist-get second :id))))
      (should (equal (plist-get (e-session-entry-previous
                                 store "session-1" (plist-get third :id))
                                :id)
                     (plist-get second :id)))
      (should (equal (plist-get (e-session-entry-next
                                 store "session-1" (plist-get second :id))
                                :id)
                     (plist-get third :id)))
      (should (equal (plist-get (e-session-latest-entry-of-type
                                 store "session-1" 'message)
                                :id)
                     (plist-get third :id)))
      (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                             (e-session-entries-from
                              store "session-1" (plist-get second :id)))
                     (list (plist-get second :id)
                           (plist-get third :id)
                           (plist-get compaction :id)))))))

(ert-deftest e-session-test-latest-valid-compaction-requires-current-boundary ()
  "Latest valid compaction ignores records with missing kept-entry boundaries."
  (let ((store (e-session-store-create)))
    (let* ((session-id (plist-get (e-session-create store :id "session-1") :id))
           (root (car (e-session-session-events store session-id)))
           (first (e-session-append-message
                   store "session-1" '(:role user :content "one")))
           (second (e-session-append-message
                    store "session-1" '(:role user :content "two"))))
      (e-session-append-compaction
       store "session-1" "invalid"
       :first-kept-entry-id "missing")
      (let ((valid
             (e-session-append-compaction
              store "session-1" "valid"
              :first-kept-entry-id (plist-get second :id))))
        (should (equal (plist-get (e-session-latest-valid-compaction
                                   store "session-1")
                                  :id)
                       (plist-get valid :id)))
        (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                               (e-session-entries-before
                                store "session-1" (plist-get second :id)))
                       (list (plist-get root :id)
                             (plist-get first :id))))))))

(ert-deftest e-session-test-current-branch-persists-through-replay ()
  "Current branch cursor records append and replay into session state."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-set-current-branch store session-id "branch-a")
          (e-session-set-current-branch store session-id "branch-b")
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (plist-get (e-session-get loaded session-id)
                                      :current-branch)
                           "branch-b"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-state-records-are-identifiable-session-events ()
  "Session state JSONL records are first-class identifiable session events."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-set-metadata store session-id '(:project-root "/tmp/project/"))
          (e-session-set-turn-options store session-id '(:model "gpt-test"))
          (e-session-set-current-branch store session-id "branch-a")
          (let* ((loaded (e-session-persistent-store-create directory))
                 (events (e-session-session-events loaded session-id))
                 (types (mapcar (lambda (event)
                                  (plist-get event :event-type))
                                events))
                 (path-types (mapcar (lambda (entry)
                                       (plist-get entry :event-type))
                                     (e-session-current-path loaded session-id))))
            (should (equal types
                           '(session-created
                             session-info
                             session-info
                             current-branch)))
            (dolist (event events)
              (should (plist-get event :id)))
            (should-not (plist-get (car events) :parent-id))
            (should (equal (mapcar (lambda (event)
                                     (plist-get event :id))
                                   (butlast events))
                           (mapcar (lambda (event)
                                     (plist-get event :parent-id))
                                   (cdr events))))
            (should (equal path-types types))))
      (delete-directory directory t))))

(ert-deftest e-session-test-current-path-supports-synthetic-branches ()
  "Current-path reconstruction can target explicit branch heads."
  (let ((store (e-session-store-create)))
    (let* ((session-id (plist-get (e-session-create store :id "session-1") :id))
           (root (car (e-session-session-events store session-id)))
           (left (e-session-append-message
                  store session-id
                  '(:role user :content "left branch")))
           (right (e-session-append-message
                   store session-id
                   (list :role 'user
                         :content "right branch"
                         :parent-id (plist-get root :id)))))
      (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                             (e-session-current-path
                              store session-id (plist-get left :id)))
                     (list (plist-get root :id)
                           (plist-get left :id))))
      (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                             (e-session-current-path
                              store session-id (plist-get right :id)))
                     (list (plist-get root :id)
                           (plist-get right :id))))
      (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                             (e-session-current-path store session-id))
                     (list (plist-get root :id)
                           (plist-get right :id)))))))

(ert-deftest e-session-test-clear-messages-is-append-only ()
  "Clearing a session empties replayed transcript without truncating JSONL."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id))
         (path (expand-file-name "session-1.jsonl"
                                 (expand-file-name "sessions" directory))))
    (unwind-protect
        (progn
          (e-session-append-message
           store session-id '(:id "msg-1" :role user :content "hello"))
          (e-session-clear-messages store session-id)
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (e-session-messages loaded session-id) nil))
            (should (string-match-p "\"type\":\"message\""
                                    (with-temp-buffer
                                      (insert-file-contents path)
                                      (buffer-string))))
            (should (string-match-p "\"type\":\"messages-cleared\""
                                    (with-temp-buffer
                                      (insert-file-contents path)
                                      (buffer-string))))))
      (delete-directory directory t))))

(ert-deftest e-session-test-clear-messages-creates-reset-boundary-root ()
  "Clearing messages makes the next message parent to the clear event."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-append-message
           store session-id '(:role user :content "old"))
          (let* ((clear-event (e-session-clear-messages store session-id))
                 (new-message
                  (e-session-append-message
                   store session-id '(:role user :content "new")))
                 (path (e-session-current-path store session-id))
                 (loaded (e-session-persistent-store-create directory))
                 (loaded-path (e-session-current-path loaded session-id)))
            (should (eq (plist-get clear-event :event-type) 'messages-cleared))
            (should (equal (plist-get new-message :parent-id)
                           (plist-get clear-event :id)))
            (should (equal (mapcar (lambda (entry)
                                     (or (plist-get entry :event-type)
                                         (plist-get entry :role)))
                                   path)
                           '(session-created messages-cleared user)))
            (should (equal (mapcar (lambda (entry)
                                     (or (plist-get entry :event-type)
                                         (plist-get entry :role)))
                                   loaded-path)
                           '(session-created messages-cleared user)))))
      (delete-directory directory t))))

(ert-deftest e-session-test-activity-events-persist-and-clear-with-messages ()
  "Activity events are durable session records and clear with transcript state."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-append-activity-event
           store
           session-id
           "turn-1"
           'reasoning-delta
           '(:content "Need current buffer state."))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (mapcar (lambda (event)
                                     (plist-get event :event-type))
                                   (e-session-activity-events loaded session-id))
                           '(reasoning-delta)))
            (should (equal (plist-get
                            (car (e-session-activity-events loaded session-id))
                            :payload)
                           '(:content "Need current buffer state.")))))
          (e-session-clear-messages store session-id)
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (e-session-activity-events loaded session-id) nil)))
      (delete-directory directory t))))

(ert-deftest e-session-test-activity-events-preserve-append-order ()
  "Activity events stay in insertion order across multiple appends."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1")
    (e-session-append-activity-event
     store "session-1" "turn-1" 'reasoning-delta '(:content "one"))
    (e-session-append-activity-event
     store "session-1" "turn-1" 'tool-started '(:name "read"))
    (should (equal (mapcar (lambda (event)
                             (plist-get event :event-type))
                           (e-session-activity-events store "session-1"))
                   '(reasoning-delta tool-started)))))

(ert-deftest e-session-test-process-reports-persist-outside-messages ()
  "Process reports replay as dedicated entries outside the transcript."
  (let* ((directory (make-temp-file "e-session-process-report-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (let ((report
                 (e-session-append-process-report
                  store "session-1"
                  '(:report-type "marker" :marker-id "marker-1"))))
            (should (eq (plist-get report :type) 'process-report))
            (should (equal (plist-get report :marker-id) "marker-1")))
          (should-not (e-session-messages store "session-1"))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (reports (e-session-process-reports loaded "session-1")))
            (should (= (length reports) 1))
            (should (equal (plist-get (car reports) :marker-id) "marker-1"))
            (should-not (e-session-messages loaded "session-1"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-append-activity-event-can-skip-index-write ()
  "Activity event append callers can skip immediate index persistence."
  (let ((store (e-session-store-create))
        (write-count 0))
    (e-session-create store :id "session-1")
    (cl-letf (((symbol-function 'e-session--write-index)
               (lambda (_store)
                 (setq write-count (1+ write-count)))))
      (e-session-append-activity-event
       store "session-1" "turn-1" 'reasoning-delta '(:content "one")
       :write-index nil)
      (should (= write-count 0))
      (e-session-append-activity-event
       store "session-1" "turn-1" 'tool-started '(:name "read")
       :write-index t)
      (should (= write-count 1)))
    (should (equal (mapcar (lambda (event)
                             (plist-get event :event-type))
                           (e-session-activity-events store "session-1"))
                   '(reasoning-delta tool-started)))))

(ert-deftest e-session-test-latest-token-usage-event-is-derived-on-append-replay-and-clear ()
  "Latest token usage is available without scanning durable activity events."
  (let* ((directory (make-temp-file "e-session-token-usage-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "session-1")
          (e-session-append-activity-event
           store "session-1" "turn-1" 'reasoning-delta '(:content "thinking"))
          (e-session-append-activity-event
           store "session-1" "turn-1" 'token-usage '(:input-tokens 10))
          (e-session-append-activity-event
           store "session-1" "turn-2" 'token-usage '(:input-tokens 20))
          (should (equal (plist-get
                          (plist-get
                           (e-session-latest-token-usage-event store "session-1")
                           :payload)
                          :input-tokens)
                         20))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (plist-get
                            (plist-get
                             (e-session-latest-token-usage-event loaded "session-1")
                             :payload)
                            :input-tokens)
                           20))
            (e-session-clear-messages loaded "session-1")
            (should-not
             (e-session-latest-token-usage-event loaded "session-1"))))
      (delete-directory directory t))))

(ert-deftest e-session-test-profile-records-persistent-appends-and-index-writes ()
  "Enabled dev profiling records persistent append and index write spans."
  (let* ((directory (make-temp-file "e-session-profile-" t))
         (profile-directory (make-temp-file "e-session-profile-trace-" t))
         (e-dev-profile-directory profile-directory)
         (e-dev-profile--enabled nil)
         (e-dev-profile--current-file nil)
         (e-dev-profile--latest-file nil)
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-dev-profile-start)
          (e-session-create store :id "session-1")
          (e-session-append-message
           store "session-1" '(:role user :content "hello" :turn-id "turn-1"))
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "session.append-record" aggregates nil nil #'equal))
            (should (alist-get "session.write-index" aggregates nil nil #'equal))))
      (delete-directory directory t)
      (delete-directory profile-directory t))))

(ert-deftest e-session-test-append-after-replay-and-clear-keeps-clean-order ()
  "Replay finalization and clear reset internal append state."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get (e-session-create store :id "session-1") :id)))
    (unwind-protect
        (progn
          (e-session-append-message
           store session-id '(:id "msg-1" :role user :content "old"))
          (e-session-append-activity-event
           store session-id "turn-1" 'reasoning-delta '(:content "old"))
          (let ((loaded (e-session-persistent-store-create directory)))
            (e-session-append-message
             loaded session-id '(:id "msg-2" :role assistant :content "replayed"))
            (should (equal (mapcar (lambda (message)
                                     (plist-get message :id))
                                   (e-session-messages loaded session-id))
                           '("msg-1" "msg-2")))
            (e-session-clear-messages loaded session-id)
            (e-session-append-message
             loaded session-id '(:id "msg-3" :role user :content "new"))
            (e-session-append-activity-event
             loaded session-id "turn-2" 'tool-started '(:name "after-clear"))
            (should (equal (mapcar (lambda (message)
                                     (plist-get message :id))
                                   (e-session-messages loaded session-id))
                           '("msg-3")))
            (should (equal (mapcar (lambda (event)
                                     (plist-get event :event-type))
                                   (e-session-activity-events loaded session-id))
                           '(tool-started)))))
      (delete-directory directory t))))

(ert-deftest e-session-test-list-sessions-sorted-with-display-metadata ()
  "Session list returns recent sessions with title, counts, and file path."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "older")
          (e-session-append-message
           store "older" '(:id "old-msg" :role user :content "older prompt"))
          (e-session-create store :id "newer")
          (e-session-append-message
           store "newer" '(:id "new-msg" :role user :content "newer prompt"))
          (e-session-rename store "newer" "Explicit title")
          (let ((sessions (e-session-list store)))
            (should (equal (mapcar (lambda (session) (plist-get session :id))
                                   sessions)
                           '("newer" "older")))
            (should (equal (plist-get (car sessions) :title)
                           "Explicit title"))
            (should (equal (plist-get (cadr sessions) :title)
                           "older prompt"))
            (should (= (plist-get (car sessions) :message-count) 1))
            (should (string-suffix-p
                     "sessions/newer.jsonl"
                     (plist-get (car sessions) :file)))))
      (delete-directory directory t))))

(ert-deftest e-session-test-list-roots-excludes-worker-sessions ()
  "Root listing omits subagent and task-queue sessions without deleting them."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "root")
    (e-session-create store :id "subagent"
                      :metadata '(:parent-session-id "root"))
    (e-session-create store :id "task"
                      :metadata '(:task-queue-task-id "tsk_000001"))
    (should (equal (mapcar (lambda (session) (plist-get session :id))
                           (e-session-list-roots store))
                   '("root")))
    (should (= (length (e-session-list store)) 3))))

(ert-deftest e-session-test-index-store-list-roots-excludes-worker-sessions ()
  "Root listing classifies unloaded indexed sessions from durable metadata."
  (let* ((directory (make-temp-file "e-session-index-roots-" t))
         (store (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "root")
          (e-session-create store :id "subagent"
                            :metadata '(:parent-session-id "root"
                                        :subagent-role "tool-user"))
          (e-session-create store :id "task"
                            :metadata '(:task-queue-task-id "tsk_000001"))
          (let ((indexed (e-session-persistent-index-store-create directory)))
            (should (equal (mapcar (lambda (session)
                                     (plist-get session :id))
                                   (e-session-list-roots indexed))
                           '("root")))
            (should (= (length (e-session-list indexed)) 3))))
      (delete-directory directory t))))

(ert-deftest e-session-test-refresh-index-metadata-repairs-unloaded-stubs ()
  "Index refresh repairs stale unloaded metadata without replacing sessions."
  (let* ((directory (make-temp-file "e-session-refresh-index-" t))
         (writer (e-session-persistent-store-create directory)))
    (unwind-protect
        (progn
          (e-session-create writer :id "root")
          (e-session-create writer :id "worker"
                            :metadata '(:parent-session-id "root"
                                        :subagent-role "tool-user"))
          (let* ((store (e-session-persistent-index-store-create directory))
                 (worker (e-session--peek-session store "worker")))
            ;; Reproduce a store retained across reload from code that did not
            ;; hydrate metadata into unloaded index stubs.
            (plist-put worker :metadata nil)
            (should (equal (mapcar (lambda (session)
                                     (plist-get session :id))
                                   (e-session-list-roots store))
                           '("worker" "root")))
            (e-session-refresh-index-metadata store)
            (should (eq (e-session--peek-session store "worker") worker))
            (should-not (plist-get worker :loaded))
            (should (equal (plist-get (plist-get worker :metadata)
                                      :parent-session-id)
                           "root"))
            (should (equal (mapcar (lambda (session)
                                     (plist-get session :id))
                                   (e-session-list-roots store))
                           '("root")))))
      (delete-directory directory t))))

(ert-deftest e-session-test-list-sessions-sorted-by-last-message ()
  "Session list order follows last message time, not metadata touches."
  (let* ((directory (make-temp-file "e-session-" t))
         (store (e-session-persistent-store-create directory))
         (timestamps '("2026-05-22T10:00:00Z"
                       "2026-05-22T10:00:01Z"
                       "2026-05-22T10:00:02Z"
                       "2026-05-22T10:00:03Z"
                       "2026-05-22T10:00:04Z")))
    (unwind-protect
        (cl-letf (((symbol-function 'e-session--timestamp)
                   (lambda (&optional _time)
                     (prog1 (car timestamps)
                       (setq timestamps (cdr timestamps))))))
          (e-session-create store :id "older")
          (e-session-append-message
           store "older" '(:id "old-msg" :role user :content "older prompt"))
          (e-session-create store :id "newer")
          (e-session-append-message
           store "newer" '(:id "new-msg" :role user :content "newer prompt"))
          (e-session-rename store "older" "Touched older title")
          (let ((ids (mapcar (lambda (session) (plist-get session :id))
                             (e-session-list store))))
            (should (equal ids '("newer" "older"))))
          (let* ((index-json
                  (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "index.json" directory))
                    (buffer-string)))
                 (newer-position (string-match "\"newer\"" index-json))
                 (older-position (string-match "\"older\"" index-json)))
            (should newer-position)
            (should older-position)
            (should (< newer-position older-position))))
      (delete-directory directory t))))

(ert-deftest e-session-test-fork-snapshots-messages-and-leaves-source-untouched ()
  "Forking seeds the fork with the source's messages and diverges independently."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "src" :metadata '(:name "Src"))
    (e-session-append-message store "src" '(:role user :content "one"))
    (e-session-append-message store "src" '(:role assistant :content "two"))
    (let* ((fork (e-session-fork store "src"))
           (fork-id (plist-get fork :id)))
      (should (not (equal fork-id "src")))
      (should (equal (mapcar (lambda (m) (plist-get m :content))
                             (e-session-messages store fork-id))
                     '("one" "two")))
      ;; New turns append only to the fork; the source is untouched.
      (e-session-append-message store fork-id '(:role user :content "three"))
      (should (= (length (e-session-messages store "src")) 2))
      (should (= (length (e-session-messages store fork-id)) 3)))))

(ert-deftest e-session-test-fork-mints-fresh-identity-and-linear-chain ()
  "Fork messages get fresh ids and a clean linear parent chain."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "src")
    (e-session-append-message store "src" '(:id "m1" :role user :content "a"))
    (e-session-append-message store "src" '(:id "m2" :role assistant :content "b"))
    (let* ((fork (e-session-fork store "src"))
           (fork-id (plist-get fork :id))
           (messages (e-session-messages store fork-id))
           (ids (mapcar (lambda (m) (plist-get m :id)) messages))
           (parents (mapcar (lambda (m) (plist-get m :parent-id)) messages)))
      ;; Fresh identity: source ids do not leak into the fork.
      (should-not (seq-intersection ids '("m1" "m2")))
      (should (cl-every #'stringp ids))
      ;; Linear chain: the second message's parent is the first's id.
      (should (equal (nth 1 parents) (nth 0 ids))))))

(ert-deftest e-session-test-fork-copies-context-metadata-and-turn-options ()
  "Fork inherits context metadata and turn options, with name override."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "src"
                      :metadata '(:name "Src" :project-root "/tmp/proj/"))
    (e-session-set-turn-options store "src" '(:model "m-1"))
    (let* ((fork (e-session-fork store "src" :name "Forked"))
           (fork-id (plist-get fork :id))
           (metadata (plist-get fork :metadata)))
      (should (equal (plist-get metadata :project-root) "/tmp/proj/"))
      (should (equal (plist-get metadata :name) "Forked"))
      (should (equal (plist-get (e-session-turn-options store fork-id) :model)
                     "m-1")))))

(ert-deftest e-session-test-fork-at-head-truncates-snapshot ()
  "Forking at an explicit head only snapshots messages up to that entry."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "src")
    (let ((first (e-session-append-message
                  store "src" '(:role user :content "keep"))))
      (e-session-append-message store "src" '(:role assistant :content "drop"))
      (let* ((fork (e-session-fork store "src" :at (plist-get first :id)))
             (fork-id (plist-get fork :id)))
        (should (equal (mapcar (lambda (m) (plist-get m :content))
                               (e-session-messages store fork-id))
                       '("keep")))))))

(provide 'e-session-test)

;;; e-session-test.el ends here
