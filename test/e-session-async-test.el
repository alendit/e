;;; e-session-async-test.el --- RDBMS-first session persistence tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-session)
(require 'e-session-async)
(require 'e-session-sqlite)
(require 'e-context-lifetime)
(require 'e-chat-service)
(require 'e-session-resources)

(defun e-session-async-test--wait (work)
  "Wait at this explicit test boundary for WORK and return its status."
  (let ((deadline (+ (float-time) 8.0)))
    (while (and (not (e-request-terminal-p (e-work-handle-lifecycle work)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (e-request-terminal-p (e-work-handle-lifecycle work)))
    (e-work-status work)))

(defun e-session-async-test--wait-finished (work)
  "Wait at this explicit test boundary and return WORK's result."
  (let ((status (e-session-async-test--wait work)))
    (should (eq (plist-get status :state) 'finished))
    (plist-get status :result)))

(defun e-session-async-test--close (store)
  "Close isolated STORE without touching a running Emacs."
  (when store
    (ignore-errors (e-session-sqlite-store-close store))))

(ert-deftest e-session-async-rdbms-public-enable-is-idempotent ()
  "The public enable seam idempotently installs the session service."
  (let* ((directory (make-temp-file "e-session-enable-" t))
         (store (e-session-sqlite-store-create directory)))
    (unwind-protect
        (progn
          (should-not (e-session-async-enabled-p store))
          (should (eq (e-session-enable store) store))
          (should (eq (e-session-enable store) store))
          (should (e-session-async-enabled-p store)))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-rdbms-create-and-append-install-no-aggregate ()
  "Commands persist through query rows without installing a session replica."
  (let* ((directory (make-temp-file "e-session-rdbms-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t)))
    (unwind-protect
        (let ((create (e-session-create
                       store :id "rdbms" :metadata '(:name "RDBMS"))))
          (should (memq (plist-get (e-work-status create) :state)
                        '(started finished)))
          (e-session-async-test--wait-finished create)
          (let ((append
                 (e-session-append-message
                  store "rdbms" '(:role user :content "hello"))))
            (should (memq (plist-get (e-work-status append) :state)
                          '(started finished)))
            (e-session-async-test--wait-finished append))
          (let ((state
                 (e-session-async-test--wait-finished
                  (e-session-async-query-state store "rdbms")))
                (page
                 (e-session-async-test--wait-finished
                  (e-session-async-visible-message-page store "rdbms" 8))))
            (should (= (plist-get state :message-count) 1))
            (should (equal (plist-get
                            (car (plist-get page :messages)) :content)
                           "hello")))
          (should (= (hash-table-count (e-session-store-sessions store)) 0)))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-rdbms-participant-admission-never-loads-aggregate ()
  "A private Board participant uses optimistic binding plus detached rows."
  (let* ((directory (make-temp-file "e-session-participant-rdbms-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store)))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-chat-service-create-session-start
            :harness harness :id "participant-root"))
          (let* ((root-binding
                  (e-chat-service-binding harness "participant-root"))
                 (board (e-chat-service-binding-board root-binding))
                 participant-work)
            (cl-letf (((symbol-function 'e-session-get)
                       (lambda (&rest _)
                         (ert-fail "Participant admission loaded an aggregate")))
                      ((symbol-function 'e-runtime-store-call)
                       (lambda (&rest _)
                         (ert-fail "Participant admission called SQLite synchronously")))
                      ((symbol-function 'e-runtime-store-await)
                       (lambda (&rest _)
                         (ert-fail "Participant admission awaited SQLite"))))
              (setq participant-work
                    (e-chat-service-create-participant-start
                     board harness :id "participant-child"
                     :pickup-selector '(:tags (grimoire-update))
                     :observer-selector :self
                     :default-tags '(grimoire-update) :default-to :self)))
            (should (e-work-handle-p participant-work))
            (should-not (plist-get (e-work-status participant-work) :error))
            (should (e-chat-service-binding harness "participant-child"))
            (should (= (hash-table-count (e-session-store-sessions store)) 0))
            (e-session-async-test--wait-finished participant-work)
            (let ((association
                   (e-session-async-test--wait-finished
                    (e-session-async-board-association
                     store "participant-child"))))
              (should (equal (plist-get association :board-id)
                             (e-board-registry-board-id board)))
              (should (equal (plist-get association :association-role)
                             "participant"))
              (should (equal
                       (plist-get
                        (plist-get association :routing-policy)
                        :default-tags)
                       '(grimoire-update)))
              (let* ((participant-id
                      (plist-get (plist-get association :routing-policy)
                                 :participant-id))
                     (participants
                      (e-runtime-store-call
                       (e-session-storage-runtime-store store) 'read
                       (list :op 'board-participant-list
                             :board-id (e-board-registry-board-id board)
                             :generation
                             (e-board-generation
                              (e-board-registry-board-source-board board))))))
                (should
                 (eq (plist-get
                      (cl-find participant-id participants
                               :key (lambda (row) (plist-get row :id))
                               :test #'equal)
                      :role)
                     'participant)))
              ;; The cross-domain commit must advance the Board adapter's
              ;; optimistic revision cursor as well as the live Board.  The
              ;; next ordinary Board write must not report a stale revision.
              (let* ((source (e-board-registry-board-source-board board))
                     (before (e-board-revision source))
                     (result
                      (e-board-storage-publish-record
                       (e-board-storage source)
                       (e-board-id source) (e-board-generation source)
                       '(:type admission-followup) nil)))
                (should (= (plist-get result :revision) (1+ before)))))
            (should (= (hash-table-count (e-session-store-sessions store)) 0))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-rdbms-participant-failure-discards-provisional-binding ()
  "A failed atomic admission leaves no live child participant or binding."
  (let* ((directory (make-temp-file "e-session-participant-fail-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store)))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-chat-service-create-session-start
            :harness harness :id "participant-failure-root"))
          (let* ((root-binding
                  (e-chat-service-binding harness "participant-failure-root"))
                 (board (e-chat-service-binding-board root-binding))
                 (before
                  (hash-table-count
                   (e-board-registry-board-participants board)))
                 work)
            (cl-letf
                (((symbol-function
                   'e-session-storage-submit-board-participant-admission)
                  (lambda (_store _session-id _records _query-delta
                                  _board-id _generation _participant on-settle
                                  &optional _pickup)
                    (funcall on-settle nil
                             '(e-session-storage-error "admission denied")))))
              (setq work
                    (e-chat-service-create-participant-start
                     board harness :id "participant-failure-child"
                     :pickup-selector '(:tags (subagent))
                     :observer-selector :self
                     :default-tags '(subagent) :default-to :self)))
            (should (eq (plist-get (e-work-status work) :state) 'failed))
            (should-not
             (e-chat-service-binding harness "participant-failure-child"))
            (should
             (= (hash-table-count
                 (e-board-registry-board-participants board))
                before))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-rdbms-root-navigation-is-one-detached-page ()
  "Persisted navigation reads summaries without installing a catalog."
  (let* ((directory (make-temp-file "e-session-navigation-rdbms-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store)))
    (unwind-protect
        (progn
          (dolist (id '("daily-old" "daily-new"))
            (e-session-async-test--wait-finished
             (e-session-create
              store :id id :metadata (list :name id)))
            (e-session-async-test--wait-finished
             (e-session-declare-board-state
              store id (format "chat:%s" id) (format "board:%s" id)
              "owner")))
          (let* ((work
                  (e-chat-service-root-session-page-start harness :limit 1))
                 (_ (e-session-async-test--wait-finished work))
                 (page (e-chat-service-root-session-page-value work))
                 (session (car (plist-get page :rows))))
            (should (= (length (plist-get page :rows)) 1))
            (should (plist-get page :next))
            (should (member (plist-get session :id)
                            '("daily-old" "daily-new")))
            (should (equal (plist-get session :board-id)
                           (format "board:%s" (plist-get session :id))))
            (should (= (hash-table-count
                        (e-session-store-sessions store))
                       0))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-rdbms-session-resources-page-without-catalog ()
  "Persistent session resources use bounded work and never sync-enumerate."
  (let* ((directory (make-temp-file "e-session-resource-rdbms-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (engine (e-session-resources--builtin-engine harness)))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "resource" :metadata '(:name "Resource")))
          (e-session-async-test--wait-finished
           (e-session-append-message
            store "resource" '(:role user :content "bounded resource")))
          (cl-letf (((symbol-function 'e-runtime-store-call)
                     (lambda (&rest _) (ert-fail "Resource reached sync call")))
                    ((symbol-function 'e-runtime-store-await)
                     (lambda (&rest _) (ert-fail "Resource awaited storage"))))
            (should-error (e-session-resources--engine-list-sessions engine)
                          :type 'e-session-resources-async-required)
            (let* ((glob-pair
                    (e-session-resources--async-glob-sessions
                     engine nil 4 t nil nil nil nil nil nil))
                   (glob-page
                    (e-session-async-test--wait-finished (car glob-pair)))
                   (glob-result (funcall (cdr glob-pair) glob-page))
                   (read-pair
                    (e-session-resources--async-read
                     engine "resource" "messages" nil))
                   (record-page
                    (e-session-async-test--wait-finished (car read-pair)))
                   (content (funcall (cdr read-pair) record-page)))
              (should (= (length (plist-get glob-result :resources)) 1))
              (should (string-match-p "bounded resource" content))
              (should (= (hash-table-count
                          (e-session-store-sessions store))
                         0)))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-rdbms-curation-uses-authoritative-generation-row ()
  "Curation validates against SQLite current state without aggregate replay."
  (let* ((directory (make-temp-file "e-session-curation-rdbms-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         (generation-id "generation:rdbms"))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "curation"))
          (let ((root
                 (plist-get
                  (e-session-async-test--wait-finished
                   (e-session-async-query-state store "curation"))
                  :current-head-id)))
            (e-session-async-test--wait-finished
             (e-session-append-context-generation
              store "curation"
              (e-context-lifetime-generation-create
               :id generation-id :checkpoint nil
               :covered-session-boundary root))))
          (let* ((frame
                  (e-context-lifetime-frame-create
                   :id "frame:rdbms" :generation-id generation-id
                   :consumer-request-id "consumer:rdbms"
                   :observations
                   '((:observation-id "observation:rdbms"
                      :kind "current-state"
                      :source-entry-ref "source:rdbms"
                      :source-fingerprint "fingerprint:rdbms"
                      :effective-delivery "inherited"
                      :body (:role system :content "bounded")))))
                 (package
                  (plist-get
                   (e-context-lifetime-prepare-curation-disposition
                    frame '(:keep (1) :summaries nil :erase nil)
                    "response:rdbms" 1.0)
                   :package)))
            (e-session-async-test--wait-finished
             (e-session-append-context-curation-package
              store "curation" package)))
          (let ((state
                 (e-session-async-test--wait-finished
                  (e-session-async-query-state store "curation"))))
            (should (equal (plist-get state
                                      :current-context-generation-id)
                           generation-id))
            (should (= (plist-get state :journal-position) 3)))
          (should (= (hash-table-count (e-session-store-sessions store)) 0)))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-rdbms-admits-independent-commands-without-prefetch ()
  "Concurrent commands submit directly without an application read/FIFO."
  (let* ((directory (make-temp-file "e-session-held-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t))
         writes reads)
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-storage-submit-owned)
                   (lambda (_store owner body on-settle &optional _escrow)
                     (let ((operation
                            (list :owner owner :body body
                                  :on-settle on-settle)))
                       (setq writes (append writes (list operation)))
                       operation)))
                  ((symbol-function 'e-session-storage-submit)
                   (lambda (_store kind body on-settle &optional _escrow)
                     (let ((operation
                            (list :kind kind :body body
                                  :on-settle on-settle)))
                       (setq reads (append reads (list operation)))
                       operation)))
                  ((symbol-function 'e-runtime-store-call)
                   (lambda (&rest _) (ert-fail "Interactive sync call")))
                  ((symbol-function 'e-runtime-store-await)
                   (lambda (&rest _) (ert-fail "Interactive await"))))
          (let ((create (e-session-create store :id "held"))
                append)
            (setq append
                  (e-session-append-message
                   store "held" '(:role user :content "queued")))
            (should (= (length writes) 2))
            (should-not reads)
            (should (= (e-session-async-pending-count store "held") 2))
            (let* ((create-write (car writes))
                   (append-write (cadr writes))
                   (create-command
                    (e-session-query-command-from-wire
                     (plist-get (plist-get create-write :body) :command)))
                   (append-command
                    (e-session-query-command-from-wire
                     (plist-get (plist-get append-write :body) :command))))
              (should (eq (plist-get (plist-get create-write :body) :op)
                          'session-command))
              (should (eq (e-session-aggregate-command-tag create-command)
                          'create))
              (should (eq (e-session-aggregate-command-tag append-command)
                          'append-message))
              (funcall (plist-get create-write :on-settle)
                       '(:id "held" :metadata nil) nil)
              (funcall (plist-get append-write :on-settle)
                       '(:id "message" :role user :content "queued") nil))
            (should (eq (plist-get (e-work-status create) :state) 'finished))
            (should (eq (plist-get (e-work-status append) :state) 'finished))
            (should (= (hash-table-count
                        (e-session-store-sessions store))
                       0))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-rdbms-write-failure-is-owner-local ()
  "One failed owner becomes suspect while a sibling still persists and reads."
  (let* ((directory (make-temp-file "e-session-owner-failure-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t)))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "failed"))
          (e-session-note-persistence-failure
           store "failed" '(e-session-storage-error "deliberate"))
          (let ((rejected
                 (e-session-append-message
                  store "failed" '(:role user :content "blocked"))))
            (should (eq (plist-get (e-work-status rejected) :state) 'failed)))
          (e-session-async-test--wait-finished
           (e-session-create store :id "healthy"))
          (e-session-async-test--wait-finished
           (e-session-append-message
            store "healthy" '(:role user :content "persisted")))
          (let ((page
                 (e-session-async-test--wait-finished
                  (e-session-async-visible-message-page store "healthy" 4))))
            (should (equal (plist-get
                            (car (plist-get page :messages)) :content)
                           "persisted"))))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-rdbms-facades-derive-relational-records ()
  "Common session facades append without loading a durable aggregate."
  (let* ((directory (make-temp-file "e-session-facades-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t)))
    (unwind-protect
        (progn
          (e-session-async-test--wait-finished
           (e-session-create store :id "facades"))
          (let ((message
                 (e-session-async-test--wait-finished
                  (e-session-append-message
                   store "facades" '(:role user :content "visible")))))
            (e-session-async-test--wait-finished
             (e-session-set-message-display
              store "facades" (plist-get message :id) 'hidden)))
          (e-session-async-test--wait-finished
           (e-session-append-process-report
            store "facades" '(:kind process :status done)))
          (e-session-async-test--wait-finished
           (e-session-append-branch-summary
            store "facades" "branch" "summary"))
          (e-session-async-test--wait-finished
           (e-session-append-compaction
            store "facades" "compacted" :branch-id "branch"))
          (e-session-async-test--wait-finished
           (e-session-append-provider-anchor
            store "facades" 'openai :model "model"))
          (e-session-async-test--wait-finished
           (e-session-declare-board-state
            store "facades" "principal" "board" "owner"))
          (let ((association
                 (e-session-async-test--wait-finished
                  (e-session-async-board-association store "facades"))))
            (should (equal (plist-get association :board-id) "board")))
          (e-session-async-test--wait-finished
           (e-session-clear-messages store "facades"))
          (should (= (plist-get
                      (e-session-async-test--wait-finished
                       (e-session-async-query-state store "facades"))
                      :message-count)
                     0))
          (should (e-session-async-test--wait-finished
                   (e-session-delete store "facades")))
          (should-not
           (e-session-async-test--wait-finished
            (e-session-async-query-state store "facades")))
          (should (= (hash-table-count (e-session-store-sessions store)) 0)))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(ert-deftest e-session-async-rdbms-legacy-aggregate-read-fails-before-io ()
  "A legacy aggregate facade cannot trigger reconstruction on async SQLite."
  (let* ((directory (make-temp-file "e-session-no-replay-" t))
         (store (e-session-sqlite-store-create directory :asynchronous t)))
    (unwind-protect
        (cl-letf (((symbol-function 'e-runtime-store-call)
                   (lambda (&rest _) (ert-fail "Aggregate read reached SQLite")))
                  ((symbol-function 'e-runtime-store-await)
                   (lambda (&rest _) (ert-fail "Aggregate read awaited SQLite"))))
          (should-error (e-session-messages store "absent")
                        :type 'e-session-storage-error)
          (should (= (hash-table-count (e-session-store-sessions store)) 0)))
      (e-session-async-test--close store)
      (delete-directory directory t))))

(provide 'e-session-async-test)

;;; e-session-async-test.el ends here
