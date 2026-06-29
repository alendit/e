;;; e-chat-session-test.el --- Tests for chat session capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for chat-session semantic actions without presentation rendering.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-chat-session)
(require 'e-harness)
(require 'e-session)

(ert-deftest e-chat-session-test-submit-validates-and-queues-prompt ()
  "Submitting validates prompt text and queues an async harness turn."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-chat-session-submit harness "session-1" "")
     :type 'user-error)
    (let ((turn-id (e-chat-session-submit harness "session-1" "hello")))
      (should (equal (plist-get (e-harness-state harness "session-1")
                                :active-turn)
                     turn-id))
      (should (equal (plist-get (car (e-harness-messages harness "session-1"))
                                :content)
                     "hello")))))

(ert-deftest e-chat-session-test-submit-preserves-explicit-metadata ()
  "Submitting can record caller metadata beyond composer references."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-chat-session-submit
     harness
     "session-1"
     "hello"
     :metadata '(:org-canvas-scope thread)
     :references '((:uri "buffer://source")))
    (let ((metadata (plist-get (car (e-harness-messages harness "session-1"))
                               :metadata)))
      (should (equal (plist-get metadata :org-canvas-scope) 'thread))
      (should (equal (plist-get metadata :references)
                     '((:uri "buffer://source")))))))

(ert-deftest e-chat-session-test-read-marker-accepts-replayed-plist ()
  "Read-marker metadata replayed from JSON plists is read and updated."
  (let ((harness (e-harness-create
                  :backend (e-backend-create :name "noop"))))
    (e-harness-create-session
     harness
     :id "session-1"
     :metadata '(:e-chat-read-markers (:chat-default "assistant-read")))
    (let ((session (e-session-get (e-harness-sessions harness) "session-1")))
      (should (equal (e-chat-session-read-marker session "chat-default")
                     "assistant-read"))
      (should (equal (e-chat-session-read-marker session :chat-default)
                     "assistant-read")))
    (e-chat-session-set-read-marker
     harness "session-1" "assistant-next" :chat-default)
    (let* ((session (e-session-get (e-harness-sessions harness) "session-1"))
           (markers (e-chat-session-read-markers
                     (plist-get session :metadata))))
      (should (equal markers
                     '(("chat-default" . "assistant-next")))))))

(ert-deftest e-chat-session-test-queue-validates-and-delegates ()
  "Queueing validates prompt text and delegates to harness queue state."
  (let* ((backend (e-backend-create
                   :name "held"
                   :start (lambda (&rest _args) nil)))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-chat-session-submit harness "session-1" "running" :delay 1.0)
    (should-error
     (e-chat-session-queue harness "session-1" "")
     :type 'user-error)
    (let ((queue-id
           (e-chat-session-queue
            harness
            "session-1"
            "queued"
            :references '((:uri "buffer://source"))
            :metadata '(:source chat-composer))))
      (let ((item (car (e-harness-queued-prompts harness "session-1"))))
        (should (equal (plist-get item :id) queue-id))
        (should (equal (plist-get item :prompt) "queued"))
        (should (equal (plist-get item :references)
                       '((:uri "buffer://source"))))
        (should (equal (plist-get item :metadata)
                       '(:source chat-composer)))))
    (e-harness-abort harness "session-1")))

(ert-deftest e-chat-session-test-steer-validates-and-delegates ()
  "Steering validates prompt text and delegates to the active harness turn."
  (let* ((backend (e-backend-create
                   :name "steerable"
                   :start (cl-function
                           (lambda (&key messages options on-item on-done
                                          on-error on-request-start)
                             (ignore messages options on-item on-done
                                     on-error)
                             (funcall on-request-start
                                      (e-backend-request-create))
                             nil))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (let ((turn-id (e-chat-session-submit harness "session-1" "running"
                                          :delay 0)))
    (should-error
     (e-chat-session-steer harness "session-1" "")
     :type 'user-error)
      (should (equal (e-chat-session-steer
                      harness
                      "session-1"
                      "focus here"
                      :metadata '(:source chat-composer))
                     turn-id)))))

(ert-deftest e-chat-session-test-abort-reset-and-rename ()
  "Chat-session actions delegate abort, reset, and rename to the harness/store."
  (let ((harness (e-harness-create
                  :backend (e-backend-create
                            :name "delayed"
                            :stream (lambda (&rest _args) nil)))))
    (e-harness-create-session harness :id "session-1")
    (e-chat-session-submit harness "session-1" "hello" :delay 1.0)
    (e-chat-session-abort harness "session-1")
    (should (equal (plist-get (e-harness-wait harness "session-1" 0.1)
                              :status)
                   'cancelled))
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "persisted"))
    (e-chat-session-reset harness "session-1")
    (should-not (e-harness-messages harness "session-1"))
    (e-chat-session-rename harness "session-1" "Renamed")
    (should (equal (e-session-display-title
                    (e-harness-sessions harness)
                    "session-1")
                   "Renamed"))))

(ert-deftest e-chat-session-test-options-and-context ()
  "Chat-session actions update options and build context preview data."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (e-chat-session-set-model harness "session-1" "gpt-test")
    (e-chat-session-set-effort harness "session-1" "high")
    (should (equal (e-harness-session-options harness "session-1")
                   '(:model "gpt-test" :reasoning-effort "high")))
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "context question"))
    (should (equal (mapcar (lambda (message)
                             (plist-get message :content))
                           (plist-get
                            (e-chat-session-context harness "session-1")
                            :messages))
                   '("context question")))))

(ert-deftest e-chat-session-test-capability-actions ()
  "The chat-session capability exposes stable shell action names."
  (let ((capability (e-chat-session-capability-create)))
    (should (eq (e-capability-id capability) 'chat-session))
    (dolist (action '(:submit :steer :queue :abort :reset :compact :rename
                      :set-model :set-effort
                      :attach-context :detach-context :context))
      (should (functionp (e-capabilities-action capability action))))))

(ert-deftest e-chat-session-test-attachments-are-current-state-context ()
  "Canvas attachments are rebuilt from current live buffer state per context."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-harness-create-session harness :id "session-1")
    (with-temp-buffer
      (rename-buffer "e-chat-session-canvas" t)
      (insert "first canvas state")
      (e-chat-session-attach-context
       harness
       "session-1"
       (list :uri (concat "buffer://" (buffer-name))
             :label "canvas"
             :buffer-name (buffer-name))
       :canvas t)
      (let ((content (plist-get (car (plist-get
                                      (e-chat-session-context
                                       harness
                                       "session-1")
                                      :messages))
                                :content)))
        (should (string-match-p "<canvas" content))
        (should (string-match-p "first canvas state" content))
        ;; The canvas guidance must steer writes to the attachment uri and
        ;; warn off look-alike helper buffers, so the model does not write to
        ;; the wrong buffer.
        (should (string-match-p "Always write to the exact uri" content))
        (should (string-match-p "e-org-canvas-input" content)))
      (erase-buffer)
      (insert "second canvas state")
      (let ((content (plist-get (car (plist-get
                                      (e-chat-session-context
                                       harness
                                       "session-1")
                                      :messages))
                                :content)))
        (should-not (string-match-p "first canvas state" content))
        (should (string-match-p "second canvas state" content))))))

(ert-deftest e-chat-session-test-file-attachment-prefers-live-buffer ()
  "File attachments read unsaved live buffers before disk contents."
  (let ((file (make-temp-file "e-chat-session-canvas-" nil ".txt"))
        (harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (unwind-protect
        (progn
          (write-region "disk state" nil file nil 'silent)
          (e-harness-activate-capability harness (e-chat-session-capability-create))
          (e-harness-create-session harness :id "session-1")
          (let ((buffer (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buffer
                  (erase-buffer)
                  (insert "unsaved live state")
                  (e-chat-session-attach-context
                   harness
                   "session-1"
                   (list :uri (concat "file://" file)
                         :label "canvas.txt"
                         :buffer-name (buffer-name))
                   :canvas t)
                  (let ((content (plist-get
                                  (car (plist-get
                                        (e-chat-session-context
                                         harness
                                         "session-1")
                                        :messages))
                                  :content)))
                    (should (string-match-p "unsaved live state" content))
                    (should-not (string-match-p "disk state" content))))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))
      (delete-file file))))

(provide 'e-chat-session-test)

;;; e-chat-session-test.el ends here
