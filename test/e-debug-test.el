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

(ert-deftest e-debug-test-display-strategy-defaults-to-popup ()
  "The debug shell defaults to the floating popup display strategy."
  (should (eq e-debug-display-strategy 'popup)))

(ert-deftest e-debug-test-installs-popup-c-g-keybindings ()
  "Reloading e-debug refreshes C-g bindings in chat and popup maps."
  (let ((e-chat-mode-map (make-sparse-keymap))
        (e-debug-popup-mode-map (make-sparse-keymap)))
    (define-key e-debug-popup-mode-map (kbd "C-g") #'e-debug--dismiss-popup)
    (e-debug--install-keybindings)
    (should (eq (lookup-key e-chat-mode-map (kbd "C-g"))
                #'e-debug--dismiss-popup-or-keyboard-quit))
    (should (eq (lookup-key e-debug-popup-mode-map (kbd "C-g"))
                #'e-debug--dismiss-popup-or-keyboard-quit))
    (should-not (lookup-key e-debug-popup-mode-map (kbd "M-3")))
    (should-not (lookup-key e-debug-popup-mode-map (kbd "s-3")))
    (should-not (lookup-key e-debug-popup-mode-map (kbd "M-0")))))

(ert-deftest e-debug-test-popup-display-shows-buffer-in-focused-posframe ()
  "The popup strategy shows the existing chat buffer in a focused posframe."
  (let ((buffer (generate-new-buffer " *e-debug-popup-test*"))
        (e-debug-display-strategy 'popup)
        shown
        focused)
    (unwind-protect
        (cl-letf (((symbol-function 'e-debug--popup-available-p)
                   (lambda () t))
                  ((symbol-function 'posframe-show)
                   (lambda (shown-buffer &rest args)
                     (setq shown (cons shown-buffer args))
                     'debug-popup-frame))
                  ((symbol-function 'frame-live-p)
                   (lambda (frame)
                     (eq frame 'debug-popup-frame)))
                  ((symbol-function 'select-frame-set-input-focus)
                   (lambda (frame)
                     (setq focused frame))))
          (e-debug--show-buffer buffer)
          (should (eq (car shown) buffer))
          (should (eq (plist-get (cdr shown) :accept-focus) t))
          (should (equal (alist-get 'tab-bar-lines
                                    (plist-get (cdr shown) :override-parameters))
                         0))
          (should (eq (plist-get (cdr shown) :poshandler)
                      'posframe-poshandler-frame-center))
          (should (eq focused 'debug-popup-frame))
          (should (eq e-debug--popup-buffer buffer))
          (should (eq e-debug--popup-frame 'debug-popup-frame))
          (with-current-buffer buffer
            (should e-debug-popup-mode)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-debug-test-popup-display-uses-root-parent-frame ()
  "The popup strategy uses a root frame parent when selected from a child frame."
  (let ((buffer (generate-new-buffer " *e-debug-popup-parent-test*"))
        shown)
    (unwind-protect
        (cl-letf (((symbol-function 'selected-frame)
                   (lambda () 'child-frame))
                  ((symbol-function 'frame-parameter)
                   (lambda (frame parameter)
                     (and (eq frame 'child-frame)
                          (eq parameter 'parent-frame)
                          'root-frame)))
                  ((symbol-function 'frame-width)
                   (lambda (&optional _frame) 120))
                  ((symbol-function 'frame-height)
                   (lambda (&optional _frame) 40))
                  ((symbol-function 'posframe-show)
                   (lambda (shown-buffer &rest args)
                     (setq shown (cons shown-buffer args))
                     'debug-popup-frame))
                  ((symbol-function 'frame-live-p)
                   (lambda (frame)
                     (eq frame 'debug-popup-frame)))
                  ((symbol-function 'select-frame-set-input-focus)
                   (lambda (_frame) nil)))
          (e-debug--show-popup buffer)
          (should (eq (plist-get (cdr shown) :parent-frame)
                      'root-frame)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-debug-test-show-buffer-refocuses-visible-popup ()
  "Showing an already visible popup refocuses it without recreating the frame."
  (let ((buffer (generate-new-buffer " *e-debug-popup-refocus-test*"))
        (e-debug-display-strategy 'popup)
        (e-debug--popup-frame 'debug-popup-frame)
        (e-debug--popup-buffer nil)
        focused)
    (unwind-protect
        (progn
          (setq e-debug--popup-buffer buffer)
          (with-current-buffer buffer
            (e-debug-popup-mode 1))
          (cl-letf (((symbol-function 'e-debug--popup-available-p)
                     (lambda () t))
                    ((symbol-function 'posframe-show)
                     (lambda (&rest _args)
                       (error "posframe-show should not be called")))
                    ((symbol-function 'frame-live-p)
                     (lambda (frame)
                       (eq frame 'debug-popup-frame)))
                    ((symbol-function 'select-frame-set-input-focus)
                     (lambda (frame)
                       (setq focused frame))))
            (e-debug--show-buffer buffer)
            (should (eq focused 'debug-popup-frame))))
      (setq e-debug--popup-buffer nil)
      (setq e-debug--popup-frame nil)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-debug-test-popup-parent-frame-key-forwards-generic-command ()
  "Popup-local keys can forward arbitrary commands to the parent frame."
  (let (called
        focused
        binding)
    (cl-letf (((symbol-function 'selected-frame)
               (lambda () 'debug-popup-frame))
              ((symbol-function 'e-debug--popup-parent-frame)
               (lambda (&optional _frame) 'parent-frame))
              ((symbol-function 'e-debug--call-interactively-in-frame)
               (lambda (command frame)
                 (setq called (list command frame))))
              ((symbol-function 'frame-live-p)
               (lambda (frame)
                 (eq frame 'debug-popup-frame)))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (frame)
                 (setq focused frame))))
      (e-debug-popup-define-parent-frame-key (kbd "M-3") 'example-workspace-command)
      (setq binding (lookup-key e-debug-popup-mode-map (kbd "M-3")))
      (should (commandp binding))
      (funcall binding)
      (should (equal called '(example-workspace-command parent-frame)))
      (should (eq focused 'debug-popup-frame)))))

(ert-deftest e-debug-test-popup-display-restores-visible-composer-cursor ()
  "The debug popup restores a visible cursor for reused chat buffers."
  (let ((buffer (generate-new-buffer " *e-debug-popup-cursor-test*"))
        (e-debug-display-strategy 'popup))
    (unwind-protect
        (cl-letf (((symbol-function 'e-debug--popup-available-p)
                   (lambda () t))
                  ((symbol-function 'posframe-show)
                   (lambda (_buffer &rest _args)
                     'debug-popup-frame))
                  ((symbol-function 'frame-live-p)
                   (lambda (frame)
                     (eq frame 'debug-popup-frame)))
                  ((symbol-function 'select-frame-set-input-focus)
                   (lambda (_frame) nil)))
          (with-current-buffer buffer
            (e-chat-mode)
            (e-chat--insert-composer "for")
            (goto-char (point-min))
            (set-mark (point-max))
            (activate-mark)
            (setq-local cursor-type nil)
            (setq-local cursor-in-non-selected-windows nil))
          (e-debug--show-buffer buffer)
          (with-current-buffer buffer
            (should (eq cursor-type 'bar))
            (should (e-chat--point-in-composer-p))
            (should-not (region-active-p))
            (should cursor-in-non-selected-windows)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-debug-test-popup-dismiss-hides-without-aborting-session ()
  "Dismissing the popup hides presentation only; the debug session continues."
  (let ((buffer (generate-new-buffer " *e-debug-popup-dismiss-test*"))
        (e-debug-display-strategy 'popup)
        (e-debug--session-id "debug-session")
        hidden
        deleted
        aborted)
    (unwind-protect
        (cl-letf (((symbol-function 'e-debug--popup-available-p)
                   (lambda () t))
                  ((symbol-function 'posframe-show)
                   (lambda (_buffer &rest _args)
                     'debug-popup-frame))
                  ((symbol-function 'select-frame-set-input-focus)
                   (lambda (_frame) nil))
                  ((symbol-function 'frame-live-p)
                   (lambda (frame)
                     (eq frame 'debug-popup-frame)))
                  ((symbol-function 'posframe-hide)
                   (lambda (hidden-buffer)
                     (setq hidden hidden-buffer)))
                  ((symbol-function 'delete-frame)
                   (lambda (frame &optional _force)
                     (setq deleted frame)))
                  ((symbol-function 'e-chat-abort)
                   (lambda ()
                     (setq aborted t))))
          (e-debug--show-buffer buffer)
          (with-current-buffer buffer
            (e-debug--dismiss-popup))
          (should (eq hidden buffer))
          (should-not aborted)
          (should (buffer-live-p buffer))
          (should (eq deleted 'debug-popup-frame))
          (should (equal e-debug--session-id "debug-session"))
          (with-current-buffer buffer
            (should-not e-debug-popup-mode)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-debug-test-popup-dismiss-closes-selected-orphan-frame ()
  "Dismissing from an untracked popup child frame closes that frame."
  (let ((buffer (generate-new-buffer " *e-debug-popup-orphan-test*"))
        (e-debug--popup-buffer nil)
        (e-debug--popup-frame nil)
        hidden
        deleted)
    (unwind-protect
        (cl-letf (((symbol-function 'selected-frame)
                   (lambda ()
                     'orphan-popup-frame))
                  ((symbol-function 'frame-parameter)
                   (lambda (_frame parameter)
                     (and (eq parameter 'parent-frame)
                          'parent-frame)))
                  ((symbol-function 'frame-live-p)
                   (lambda (frame)
                     (eq frame 'orphan-popup-frame)))
                  ((symbol-function 'posframe-hide)
                   (lambda (hidden-buffer)
                     (setq hidden hidden-buffer)))
                  ((symbol-function 'delete-frame)
                   (lambda (frame &optional _force)
                     (setq deleted frame))))
          (with-current-buffer buffer
            (e-chat-mode)
            (e-debug-popup-mode 1)
            (e-debug--dismiss-popup)
            (should (eq hidden buffer))
            (should (eq deleted 'orphan-popup-frame))
            (should-not e-debug-popup-mode))
          (should (buffer-live-p buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-debug-test-chat-c-g-closes-debug-child-frame-without-popup-mode ()
  "The debug chat C-g fallback closes an orphaned debug child frame."
  (e-debug-test--with-empty-harness-registry
    (let* ((store (e-session-store-create))
           (harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)
                     :sessions store))
           (buffer (generate-new-buffer " *e-debug-popup-c-g-test*"))
           (e-debug--popup-buffer nil)
           (e-debug--popup-frame nil)
           hidden
           deleted)
      (e-harness-create-session harness :id "debug-session"
                                :metadata '(:source e-debug))
      (unwind-protect
          (cl-letf (((symbol-function 'selected-frame)
                     (lambda ()
                       'orphan-popup-frame))
                    ((symbol-function 'frame-parameter)
                     (lambda (_frame parameter)
                       (and (eq parameter 'parent-frame)
                            'parent-frame)))
                    ((symbol-function 'frame-live-p)
                     (lambda (frame)
                       (eq frame 'orphan-popup-frame)))
                    ((symbol-function 'posframe-hide)
                     (lambda (hidden-buffer)
                       (setq hidden hidden-buffer)))
                    ((symbol-function 'delete-frame)
                     (lambda (frame &optional _force)
                       (setq deleted frame))))
            (with-current-buffer buffer
              (e-chat-mode)
              (setq-local e-chat-harness harness)
              (setq-local e-chat-session-id "debug-session")
              (e-debug--dismiss-popup-or-keyboard-quit)
              (should (eq hidden buffer))
              (should (eq deleted 'orphan-popup-frame))
              (should-not e-debug-popup-mode))
            (should (buffer-live-p buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest e-debug-test-command-reattaches-popup-after-dismissal ()
  "Reopening e-debug after dismissal reuses the standing session buffer."
  (e-debug-test--with-empty-harness-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions (e-session-store-create)))
          (e-debug-display-strategy 'popup)
          (e-debug--session-id nil)
          hidden
          deleted)
      (cl-letf (((symbol-function 'e-debug--default-harness)
                 (lambda () harness))
                ((symbol-function 'e-debug--popup-available-p)
                 (lambda () t))
                ((symbol-function 'posframe-show)
                 (lambda (_buffer &rest _args)
                   'debug-popup-frame))
                ((symbol-function 'frame-live-p)
                 (lambda (frame)
                   (eq frame 'debug-popup-frame)))
                ((symbol-function 'select-frame-set-input-focus)
                 (lambda (_frame) nil))
                ((symbol-function 'posframe-hide)
                 (lambda (hidden-buffer)
                   (setq hidden hidden-buffer)))
                ((symbol-function 'delete-frame)
                 (lambda (frame &optional _force)
                   (setq deleted frame))))
        (let ((first-buffer (e-debug))
              second-buffer)
          (with-current-buffer first-buffer
            (e-debug--dismiss-popup))
          (setq second-buffer (e-debug))
          (should (eq hidden first-buffer))
          (should (eq deleted 'debug-popup-frame))
          (should (eq second-buffer first-buffer))
          (should (= (length (e-harness-session-list harness)) 1))
          (with-current-buffer second-buffer
            (should e-debug-popup-mode)))))))

(ert-deftest e-debug-test-background-turn-finished-notifies-after-popup-dismissal ()
  "A completed debug turn reports in the echo area when the popup is hidden."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         messages
         (e-debug--popup-buffer nil)
         (e-debug--popup-frame nil)
         (e-debug--notification-harness nil)
         (e-debug--notification-subscription nil))
    (e-harness-create-session harness :id "debug-session"
                              :metadata '(:source e-debug))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (e-debug--ensure-notification-subscription harness "debug-session")
      (e-harness--emit-turn-event harness "debug-session" "turn-1"
                                  'turn-finished nil)
      (should (equal (car messages) "*e-debug*: finished: turn-1")))))

(ert-deftest e-debug-test-background-turn-failed-notifies-error-after-popup-dismissal ()
  "A failed debug turn reports the compact error when the popup is hidden."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         messages
         (e-debug--popup-buffer nil)
         (e-debug--popup-frame nil)
         (e-debug--notification-harness nil)
         (e-debug--notification-subscription nil))
    (e-harness-create-session harness :id "debug-session"
                              :metadata '(:source e-debug))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (e-debug--ensure-notification-subscription harness "debug-session")
      (e-harness--emit-turn-event harness "debug-session" "turn-1"
                                  'turn-failed '(:error "provider failed"))
      (should (equal (car messages) "*e-debug*: failed: provider failed")))))

(ert-deftest e-debug-test-popup-visible-requires-live-frame ()
  "A stale popup mode does not count as visible after its frame is gone."
  (let ((buffer (generate-new-buffer " *e-debug-popup-stale-frame-test*"))
        (e-debug--popup-frame 'dead-frame))
    (unwind-protect
        (progn
          (setq e-debug--popup-buffer buffer)
          (with-current-buffer buffer
            (e-debug-popup-mode 1))
          (cl-letf (((symbol-function 'frame-live-p)
                     (lambda (frame)
                       (not (eq frame 'dead-frame)))))
            (should-not (e-debug--popup-visible-p))))
      (setq e-debug--popup-buffer nil)
      (setq e-debug--popup-frame nil)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-debug-test-notification-subscription-updates-for-new-session ()
  "Notification subscriptions follow a changed debug session id on one harness."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-debug--notification-harness nil)
         (e-debug--notification-subscription nil)
         (e-debug--notification-session-id nil))
    (e-harness-create-session harness :id "debug-session-1"
                              :metadata '(:source e-debug))
    (e-harness-create-session harness :id "debug-session-2"
                              :metadata '(:source e-debug))
    (e-debug--ensure-notification-subscription harness "debug-session-1")
    (let ((first e-debug--notification-subscription))
      (e-debug--ensure-notification-subscription harness "debug-session-2")
      (should-not (eq e-debug--notification-subscription first))
      (should (equal e-debug--notification-session-id "debug-session-2"))
      (should (equal (plist-get e-debug--notification-subscription :session-id)
                     "debug-session-2")))))

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
