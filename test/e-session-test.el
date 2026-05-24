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
           :range '(:from "msg-1" :to "msg-9"))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (compaction (car (plist-get
                                   (e-session-get loaded session-id)
                                   :compactions))))
            (should (equal (plist-get compaction :summary)
                           "Compacted early transcript."))
            (should (equal (plist-get compaction :branch-id) "branch-a"))
            (should (equal (plist-get compaction :range)
                           '(:from "msg-1" :to "msg-9")))))
      (delete-directory directory t))))

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
