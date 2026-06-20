;;; e-debug-test.el --- Tests for standing debug agent session -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for the debug agent shell's standing session resolver.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'e)
(require 'e-backend)
(require 'e-chat)
(require 'e-context-inspection)
(require 'e-debug)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-session)
(require 'e-shells)

(defmacro e-debug-test--with-empty-harness-registry (&rest body)
  "Run BODY with an isolated harness registry."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal)))
     ,@body))

(ert-deftest e-debug-test-ensure-session-reuses-standing-session ()
  "The debug resolver reuses the same standing session."
  (e-debug-test--with-empty-harness-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions (e-session-store-create)))
          (e-debug--session-id nil))
      (cl-letf (((symbol-function 'e-debug--default-harness)
                 (lambda () harness)))
        (let ((first (e-debug--ensure-session))
              (second (e-debug--ensure-session)))
          (should (equal second first))
          (should (= (length (e-harness-session-list harness)) 1))
          (should (equal (plist-get
                          (plist-get (e-session-get
                                      (e-harness-sessions harness)
                                      first)
                                     :metadata)
                          :source)
                         'e-debug)))))))

(ert-deftest e-debug-test-ensure-session-rediscovers-existing-session ()
  "The debug resolver finds an existing debug session when its cache is empty."
  (e-debug-test--with-empty-harness-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions (e-session-store-create)))
          (e-debug--session-id nil))
      (cl-letf (((symbol-function 'e-debug--default-harness)
                 (lambda () harness)))
        (let ((created (e-debug--ensure-session)))
          (setq e-debug--session-id nil)
          (should (equal (e-debug--ensure-session) created))
          (should (= (length (e-harness-session-list harness)) 1)))))))

(ert-deftest e-debug-test-command-opens-and-shows-standing-session ()
  "The `e-debug' command opens the standing debug session through chat UI."
  (e-debug-test--with-empty-harness-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions (e-session-store-create)))
          shown-buffer
          (e-debug--session-id nil))
      (cl-letf (((symbol-function 'e-debug--default-harness)
                 (lambda () harness))
                ((symbol-function 'e-debug--show-buffer)
                 (lambda (buffer)
                   (setq shown-buffer buffer))))
        (let ((buffer (e-debug)))
          (should (eq shown-buffer buffer))
          (with-current-buffer buffer
            (should (derived-mode-p 'e-chat-mode))
            (should (eq e-chat-harness harness))
            (should (equal e-chat-session-id e-debug--session-id))))))))

(ert-deftest e-debug-test-tab-display-strategy-creates-named-tab-before-buffer ()
  "The tab display strategy creates and names a debug tab before showing BUFFER."
  (let ((buffer (generate-new-buffer " *e-debug-test*"))
        (e-debug-display-strategy 'tab)
        events)
    (unwind-protect
        (cl-letf (((symbol-function 'tab-bar-tabs)
                   (lambda () '((current-tab (name . "main")))))
                  ((symbol-function 'tab-bar-new-tab)
                   (lambda (&rest _args)
                     (push 'new-tab events)))
                  ((symbol-function 'tab-bar-rename-tab)
                   (lambda (name)
                     (push (list 'rename name) events)))
                  ((symbol-function 'e-chat--pop-to-buffer)
                   (lambda (shown)
                     (push (list 'show shown) events))))
          (e-debug--show-buffer buffer)
          (should (equal (nreverse events)
                         (list 'new-tab
                               (list 'rename e-debug-tab-name)
                               (list 'show buffer)))))
      (kill-buffer buffer))))

(ert-deftest e-debug-test-tab-display-strategy-reuses-named-debug-tab ()
  "The tab display strategy reuses the named debug tab when it exists."
  (let ((buffer (generate-new-buffer " *e-debug-test*"))
        (e-debug-display-strategy 'tab)
        events)
    (unwind-protect
        (cl-letf (((symbol-function 'tab-bar-tabs)
                   (lambda () `((current-tab (name . "main"))
                                ((name . ,e-debug-tab-name)))))
                  ((symbol-function 'tab-bar-new-tab)
                   (lambda (&rest _args)
                     (ert-fail "Existing debug tab should be reused")))
                  ((symbol-function 'tab-bar-select-tab-by-name)
                   (lambda (name)
                     (push (list 'select name) events)))
                  ((symbol-function 'e-chat--pop-to-buffer)
                   (lambda (shown)
                     (push (list 'show shown) events))))
          (e-debug--show-buffer buffer)
          (should (equal (nreverse events)
                         (list (list 'select e-debug-tab-name)
                               (list 'show buffer)))))
      (kill-buffer buffer))))

(ert-deftest e-debug-test-shell-manifest-exposes-debug-command ()
  "The debug shell exposes the standing debug command."
  (let* ((shell (e-debug-shell))
         (command (e-shell-command-by-id shell 'open)))
    (should (eq (e-shell-id shell) 'debug))
    (should (equal (e-shell-required-capabilities shell)
                   '(chat-session debug-agent)))
    (should command)
    (should (eq (e-shell-command-interactive command) 'e-debug))
    (should (commandp (e-shell-command-interactive command)))))

(ert-deftest e-debug-test-capture-builds-source-and-failure-references ()
  "Debug capture includes focused source and recent failure detail references."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store)))
    (e-harness-create-session harness :id "failed-session"
                              :metadata '(:project-root "/tmp/project/"))
    (e-session-append-message
     store "failed-session"
     '(:id "msg-1" :role user :content "broken prompt" :turn-id "turn-1"))
    (e-session-append-activity-event
     store "failed-session" "turn-1" 'turn-failed
     '(:error "provider failed" :details (:status 520)))
    (with-temp-buffer
      (insert "alpha\nbeta\ngamma\n")
      (goto-char (point-min))
      (forward-line 1)
      (let* ((capture (e-debug--capture
                       :question ""
                       :inspection-harness harness))
             (prompt (plist-get capture :prompt))
             (references (plist-get capture :references)))
        (should (string-match-p "Debug what just happened here\\." prompt))
        (should (string-match-p "Diagnose first" prompt))
        (should (string-match-p "\\[source\\]" prompt))
        (should (string-match-p "\\[failure-1\\]" prompt))
        (should (string-match-p "provider failed" prompt))
        (should (= (length references) 2))
        (should (equal (plist-get (car references) :id) "source"))
        (should (equal (plist-get (cadr references) :id) "failure-1"))))))

(ert-deftest e-debug-test-here-submits-to-standing-session ()
  "`e-debug-here' submits the assembled prompt to the standing debug session."
  (let* ((debug-store (e-session-store-create))
         (debug-harness (e-harness-create
                         :backend (e-backend-fake-create :items nil)
                         :sessions debug-store))
         (inspection-harness (e-harness-create
                              :backend (e-backend-fake-create :items nil)
                              :sessions (e-session-store-create)))
         submitted
         shown-buffer
         (e-debug--session-id nil))
    (cl-letf (((symbol-function 'e-debug--default-harness)
               (lambda () debug-harness))
              ((symbol-function 'e-debug--inspection-harness)
               (lambda () inspection-harness))
              ((symbol-function 'read-string)
               (lambda (&rest _args) "why did it fail?"))
              ((symbol-function 'e-chat-submit-session)
               (lambda (harness session-id prompt &rest args)
                 (setq submitted
                       (list :harness harness
                             :session-id session-id
                             :prompt prompt
                             :references (plist-get args :references)
                             :metadata (plist-get args :metadata)))))
              ((symbol-function 'e-debug--show-buffer)
               (lambda (buffer)
                 (setq shown-buffer buffer))))
      (let ((session-id (e-debug-here)))
        (should (equal session-id e-debug--session-id))
        (should (eq (plist-get submitted :harness) debug-harness))
        (should (equal (plist-get submitted :session-id) session-id))
        (should (string-match-p "why did it fail\\?" (plist-get submitted :prompt)))
        (should (equal (plist-get (plist-get submitted :metadata) :source)
                       'e-debug-here))
        (should shown-buffer)))))

(ert-deftest e-debug-test-here-carries-chat-session-identity-without-failure ()
  "`e-debug-here' records the inspected chat session even without failures."
  (let* ((debug-harness (e-harness-create
                         :backend (e-backend-fake-create :items nil)
                         :sessions (e-session-store-create)))
         (chat-harness (e-harness-create
                        :backend (e-backend-fake-create :items nil)
                        :sessions (e-session-store-create)))
         submitted
         (e-debug--session-id nil))
    (e-harness-create-session chat-harness :id "plain-session"
                              :metadata '(:project-root "/tmp/project/"))
    (cl-letf (((symbol-function 'e-debug--default-harness)
               (lambda () debug-harness))
              ((symbol-function 'e-chat-submit-session)
               (lambda (_harness _session-id _prompt &rest args)
                 (setq submitted
                       (list :references (plist-get args :references)
                             :metadata (plist-get args :metadata)))))
              ((symbol-function 'e-debug--show-buffer)
               (lambda (_buffer) nil)))
      (let ((chat-buffer (e-chat-open :harness chat-harness
                                      :session-id "plain-session")))
        (unwind-protect
            (with-current-buffer chat-buffer
              (e-debug-here "inspect this chat")
              (should (equal (plist-get (plist-get submitted :metadata)
                                        :inspection-session-id)
                             "plain-session"))
              (should (seq-find
                       (lambda (reference)
                         (equal (plist-get reference :id)
                                "inspected-session"))
                       (plist-get submitted :references))))
          (when (buffer-live-p chat-buffer)
            (kill-buffer chat-buffer)))))))

(provide 'e-debug-test)

;;; e-debug-test.el ends here
