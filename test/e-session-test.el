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
(require 'e-session)

(ert-deftest e-session-test-create-and-read ()
  "Sessions can be created and read by id."
  (let ((store (e-session-store-create)))
    (e-session-create store :id "session-1" :metadata '(:model "fake"))
    (should (equal (plist-get (e-session-get store "session-1") :id) "session-1"))
    (should (equal (e-session-messages store "session-1") nil))))

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

(ert-deftest e-session-test-missing-session-surfaces-error ()
  "Appending to a missing session surfaces a domain error."
  (let ((store (e-session-store-create)))
    (should-error
     (e-session-append-message store "missing" '(:role user :content "x"))
     :type 'e-session-missing)))

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
           '(:model "gpt-test" :reasoning-effort "high"))
          (let ((loaded (e-session-persistent-store-create directory)))
            (should (equal (e-session-turn-options loaded session-id)
                           '(:model "gpt-test" :reasoning-effort "high")))))
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

(provide 'e-session-test)

;;; e-session-test.el ends here
