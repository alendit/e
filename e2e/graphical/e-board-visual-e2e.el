;;; e-board-visual-e2e.el --- Cross-callback WebKit Board acceptance -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; WebKit navigation and script callbacks require the server eval that opened
;; the xwidget to return.  The shell runner drives these bounded phases through
;; separate requests against one private graphical Emacs process.

;;; Code:

(require 'json)
(load (expand-file-name "e-board-activity-behavior-test.el"
                        (file-name-directory load-file-name)) nil nil t)

(defvar e-board-visual-e2e--fixture nil)
(defvar e-board-visual-e2e--buffer nil)
(defvar e-board-visual-e2e--web-state nil)
(defvar e-board-visual-e2e--expected-font-file nil)
(defvar e-board-visual-e2e--chat-state nil)
(defvar e-board-visual-e2e--delivery nil)
(defvar e-board-visual-e2e--child-store nil)
(defvar e-board-visual-e2e--child-directory nil)
(defvar e-board-visual-e2e--original-live nil)

(defun e-board-visual-e2e--widget ()
  "Return the live WebKit widget owned by the accepted Board buffer."
  (let ((widget
         (with-current-buffer e-board-visual-e2e--buffer
           (plist-get e-board-activity-visual--egui-session :xwidget))))
    (unless (xwidget-live-p widget)
      (error "Board WebKit widget is not live"))
    widget))

(defun e-board-visual-e2e--hud-in-output-p (popup fixture)
  "Return whether POPUP fits the right edge of FIXTURE's chat output."
  (let* ((output (car (e-chat-behavior-test--fixture-windows fixture)))
         (edges (window-pixel-edges output))
         (position (frame-position popup))
         (right (nth 2 edges))
         (top (nth 1 edges)))
    (and (<= (abs (- (cdr position) (+ top 12))) 8)
         (<= (abs (- right (+ (car position) (frame-pixel-width popup))
                     12))
             (+ 8 (frame-char-width popup)))
         (>= (car position) (car edges))
         (<= (+ (cdr position) (frame-pixel-height popup))
             (nth 3 edges)))))

(defun e-board-visual-e2e-start ()
  "Open a disposable owner chat and its real WebKit Board view."
  (unless (featurep 'xwidget-internal)
    (error "This Emacs has no native xwidget support"))
  (setq e-board-visual-e2e--fixture (e-chat-behavior-test--open-surface t))
  (let* ((fixture e-board-visual-e2e--fixture)
         (transcript (plist-get fixture :transcript))
         (chat-frame (selected-frame)))
    (when-let* ((reason (e-board-activity-visual-unavailable-reason)))
      (error "Board visual assets unavailable: %s" reason))
    (let ((auto-hud (get-buffer e-board-activity-visual-buffer-name)))
      (unless (and (buffer-live-p auto-hud)
                   (e-board-activity-visual-visible-for-owner-p transcript)
                   (eq (selected-frame) chat-frame)
                   (with-current-buffer auto-hud
                     (and e-board-activity-visual--compact
                          (null e-board-activity-visual--selected-run-id)
                          (e-board-visual-e2e--hud-in-output-p
                           e-board-activity-shell--popup-frame fixture)
                          (<= (frame-pixel-height
                               e-board-activity-shell--popup-frame)
                              (+ e-board-activity-hud-idle-height
                                 (frame-char-height
                                  e-board-activity-shell--popup-frame))))))
        (error "Opening the chat did not automatically show its empty Board HUD"))
      (setq e-board-visual-e2e--buffer auto-hud
            e-board-visual-e2e--expected-font-file
            (with-current-buffer auto-hud
              (alist-get 'fontFile (e-board-activity-visual--snapshot))))
      (when (and (eq system-type 'darwin)
                 (equal (face-attribute 'default :family chat-frame 'default)
                        "Menlo")
                 (not (equal e-board-visual-e2e--expected-font-file
                             "/System/Library/Fonts/Menlo.ttc")))
        (error "The HUD did not select the configured Menlo font")))
    t))

(defun e-board-visual-e2e-place-other-pane-above-chat ()
  "Move the chat below an unrelated buffer and verify its HUD follows."
  (let* ((fixture e-board-visual-e2e--fixture)
         (output (car (e-chat-behavior-test--fixture-windows fixture)))
         (upper (split-window (window-atom-root output) nil 'above))
         (other (get-buffer "*e graphical outside*")))
    (set-window-buffer upper other)
    (condition-case err
        (e-graphical-test-wait-until
         (lambda ()
           (let* ((output (car (e-chat-behavior-test--fixture-windows fixture)))
                  (popup (with-current-buffer e-board-visual-e2e--buffer
                           e-board-activity-shell--popup-frame)))
             (and (> (nth 1 (window-pixel-edges output)) 100)
                  (e-board-visual-e2e--hud-in-output-p popup fixture))))
         3.0 "HUD inside lower chat output window")
      (error
       (let* ((output (car (e-chat-behavior-test--fixture-windows fixture)))
              (popup (with-current-buffer e-board-visual-e2e--buffer
                       e-board-activity-shell--popup-frame)))
         (error "%s: output %S, popup %S size %S"
                (error-message-string err)
                (window-pixel-edges output)
                (frame-position popup)
                (cons (frame-pixel-width popup)
                      (frame-pixel-height popup))))))
    t))

(defun e-board-visual-e2e-populate ()
  "Dismiss the empty HUD, then reopen it for a two-task Board run."
  (let* ((fixture e-board-visual-e2e--fixture)
         (harness (plist-get fixture :harness))
         (session-id (plist-get fixture :session-id))
         (transcript (plist-get fixture :transcript))
         (binding (e-chat-service-binding harness session-id))
         (target (e-chat-service-publication-target binding))
         (board-id (e-chat-service-binding-board-id binding))
         (chat-frame (selected-frame))
         (composer-window
          (cdr (e-chat-behavior-test--fixture-windows fixture)))
         (live (e-subagent-live-create))
         (capture (list nil))
         child-harness run-bound)
    (with-current-buffer e-board-visual-e2e--buffer
        (e-board-activity-shell-dismiss))
    (unless (and (with-current-buffer transcript
                   e-chat--board-hud-dismissed)
                 (not (with-current-buffer e-board-visual-e2e--buffer
                        (frame-visible-p
                         e-board-activity-shell--popup-frame))))
      (error "Dismissing the automatic Board HUD did not honor chat preference"))
    (setq e-board-visual-e2e--child-directory
          (make-temp-file "e-board-visual-child-" t)
          e-board-visual-e2e--child-store
          (e-session-sqlite-store-create
           e-board-visual-e2e--child-directory :asynchronous t)
          e-board-visual-e2e--original-live e-subagent-actions-default-live
          e-subagent-actions-default-live live)
    (e-harness-instance-register
     :id :visual-e2e-worker :kind 'reviewer :subagent t
     :factory (lambda ()
                (setq child-harness
                      (e-harness-create
                       :backend (e-backend-fake-create :items nil)
                       :sessions e-board-visual-e2e--child-store))))
    (setq run-bound
          (e-subagent-spawn
           live harness session-id
           :source-turn-id "visual-parent-turn"
           :type :visual-e2e-worker
           :prompt "Hold a run-bound visual child turn."
           :label "Slack" :run-id "visual-run" :task-key "slack" :attempt 0
           :runner (e-board-activity-behavior-test--runner capture)))
    (unless (stringp (plist-get run-bound :participant-id))
      (error "Visual run-bound child was not admitted"))
    (e-board-activity-behavior-test--publish-fact
     target
     '(:version 1 :type manifest :idempotency-key "manifest:visual-run"
       :payload (:run-id "visual-run" :descriptor (:label "Daily update")
                 :tasks ((:task-key "calendar" :required t
                           :accepted-attempt 0)
                         (:task-key "slack" :required nil
                           :accepted-attempt 0))
                 :deadline (:kind none))))
    (e-board-activity-behavior-test--publish-fact
     target
     '(:version 1 :type task-attempt
       :idempotency-key "attempt:visual-run:calendar:0:queued"
       :payload (:run-id "visual-run" :task-key "calendar"
                 :attempt 0 :status queued)))
    (e-board-activity-behavior-test--publish-fact
     target
     '(:version 1 :type task-attempt
       :idempotency-key "attempt:visual-run:slack:0:running"
       :payload (:run-id "visual-run" :task-key "slack"
                 :attempt 0 :status running)))
    (e-graphical-test-wait-until
     (lambda ()
       (e-subagent-live-get live board-id
                            (plist-get run-bound :participant-id)))
     5.0 "visual child live admission")
    (e-board-activity-behavior-test--append-assignment-input
     target "visual-run" "slack" 0
     (plist-get run-bound :participant-id) "Slack")
    (e-graphical-test-wait-until
     (lambda ()
       (with-current-buffer transcript
         (and (equal (plist-get e-chat-surface--board-status
                                :selected-run-id)
                     "visual-run")
              (functionp e-chat-surface--board-status-action))))
     5.0 "visual run in public chat status")
    (setq e-board-visual-e2e--buffer
          (with-current-buffer transcript
            (funcall e-chat-surface--board-status-action)))
    (let ((popup (with-current-buffer e-board-visual-e2e--buffer
                   e-board-activity-shell--popup-frame)))
      (unless (and (eq (selected-frame) chat-frame)
                   (eq (frame-parameter popup 'parent-frame) chat-frame)
                   (frame-visible-p popup)
                   (frame-parameter popup 'no-accept-focus)
                   (>= (frame-pixel-width popup)
                       (- e-board-activity-hud-width
                          (* 2 (frame-char-width popup))))
                   (<= (frame-pixel-width popup)
                       (+ e-board-activity-hud-width (frame-char-width popup)))
                   (<= (frame-pixel-height popup)
                       (+ e-board-activity-hud-height (frame-char-height popup)))
                   (< (frame-pixel-width popup)
                      (/ (frame-pixel-width chat-frame) 2))
                   (e-board-visual-e2e--hud-in-output-p popup fixture)
                   (eq (window-buffer (frame-root-window popup))
                       e-board-visual-e2e--buffer))
        (error "Chat status did not open a compact HUD in its output window: %S"
               (list :selected (selected-frame)
                     :popup popup :position (frame-position popup)
                     :size (cons (frame-pixel-width popup)
                                 (frame-pixel-height popup))))))
    (e-board-visual-e2e--widget)
    (unless (and (window-live-p composer-window)
                 (with-current-buffer (window-buffer composer-window)
                   (e-chat-composer-active-p)))
      (error "Owner chat composer is not active beside the visual Board"))
    (with-selected-window composer-window
      (e-graphical-test-type-text "draft while visual Board is open")
      (unless (string-suffix-p
               "draft while visual Board is open"
               (buffer-substring-no-properties (point-min) (point-max)))
        (error "Composer did not accept input beside the visual Board")))
    (e-graphical-test-wait-until
     (lambda ()
       (with-current-buffer e-board-visual-e2e--buffer
         (and (eq e-board-activity-visual--detail-state 'ready)
              (equal e-board-activity-visual--selected-run-id "visual-run")
              (= (length (plist-get e-board-activity-visual--detail-page
                                    :tasks))
                 2))))
     5.0 "visual Board durable task page")
    (let* ((snapshot
            (with-current-buffer e-board-visual-e2e--buffer
              (e-board-activity-visual--snapshot)))
           (detail (alist-get 'detail snapshot))
           (required (alist-get 'requiredTasks detail))
           (optional (alist-get 'optionalTasks detail)))
      (unless (and (= (length required) 1)
                   (= (length optional) 1)
                   (equal (alist-get 'taskKey (aref required 0)) "calendar")
                   (null (alist-get 'participantId (aref required 0)))
                   (equal (alist-get 'taskKey (aref optional 0)) "slack")
                   (equal (alist-get 'participantId (aref optional 0))
                          (plist-get run-bound :participant-id))
                   (eq (alist-get
                        'canSteer (alist-get 'controls (aref optional 0)))
                       t))
        (error "Visual Board snapshot lost pending or optional task")))
    t))

(defun e-board-visual-e2e-probe-web ()
  "Request one asynchronous WebKit bootstrap observation."
  (setq e-board-visual-e2e--web-state nil)
  (xwidget-webkit-execute-script
   (e-board-visual-e2e--widget)
   (concat
    "JSON.stringify({url:location.href,ready:document.readyState,"
    "push:!!window.eguiPushState,"
    "canvas:!!document.getElementById('egui-canvas'),"
    "drawn:!!document.querySelector('#egui-canvas.ready'),"
    "stateReceived:!!window.eguiBoardStateReady,"
    "fontReady:!!window.eguiBoardFontReady,"
    "fontError:window.eguiBoardFontError||null})")
   (lambda (value)
     (when (stringp value)
       (setq e-board-visual-e2e--web-state
             (json-read-from-string value)))))
  t)

(defun e-board-visual-e2e-web-ready-p ()
  "Return whether WebKit loaded the Board WASM app and its canvas."
  (let ((state e-board-visual-e2e--web-state))
    (and (string-prefix-p "http://127.0.0.1:"
                          (or (alist-get 'url state) ""))
         (equal (alist-get 'ready state) "complete")
         (eq (alist-get 'push state) t)
         (eq (alist-get 'canvas state) t)
         (eq (alist-get 'drawn state) t)
         (eq (alist-get 'stateReceived state) t))))

(defun e-board-visual-e2e-font-settled-p ()
  "Return whether the configured font loaded or reported an error."
  (or (eq (alist-get 'fontReady e-board-visual-e2e--web-state) t)
      (alist-get 'fontError e-board-visual-e2e--web-state)))

(defun e-board-visual-e2e-font-ready-p ()
  "Assert that WebKit applied the configured Emacs font."
  (when-let* ((failure (alist-get 'fontError e-board-visual-e2e--web-state)))
    (error "Board font load failed: %s" failure))
  (eq (alist-get 'fontReady e-board-visual-e2e--web-state) t))

(defun e-board-visual-e2e-install-state-observer ()
  "Capture the next real Emacs-to-WebKit Board snapshot."
  (xwidget-webkit-execute-script
   (e-board-visual-e2e--widget)
   (concat
    "window.__eBoardStates=[];"
    "window.__eBoardOriginalPush=window.eguiPushState;"
    "window.eguiPushState=function(json){"
    "window.__eBoardStates.push(JSON.parse(json));"
    "return window.__eBoardOriginalPush(json);};"))
  t)

(defun e-board-visual-e2e-submit-chat-message ()
  "Submit an ordinary message through the chat composer."
  (e-chat-behavior-test--submit e-board-visual-e2e--fixture
                                "Show a short answer")
  (with-current-buffer e-board-visual-e2e--buffer
    (unless (null e-board-activity-visual--selected-run-id)
      (error "Ordinary chat submission selected Board work")))
  t)

(defun e-board-visual-e2e-stream-chat-reply ()
  "Stream a provider reply to the submitted chat message."
  (e-graphical-test-stream-emit
   (plist-get e-board-visual-e2e--fixture :stream)
   '(:type assistant-delta :content "An ordinary chat reply"))
  (e-graphical-test-wait-until
   (lambda ()
     (with-current-buffer
         (plist-get e-board-visual-e2e--fixture :transcript)
       (equal (e-chat-surface-status) "streaming")))
   2.0 "chat streaming status")
  t)

(defun e-board-visual-e2e-finish-chat-reply ()
  "Settle the ordinary chat turn without creating Board work."
  (e-chat-behavior-test--finish e-board-visual-e2e--fixture
                                "An ordinary chat reply")
  t)

(defun e-board-visual-e2e-probe-chat-status ()
  "Read the most recent chat status delivered to the real WebKit page."
  (setq e-board-visual-e2e--chat-state nil)
  (xwidget-webkit-execute-script
   (e-board-visual-e2e--widget)
   (concat
    "(function(){const states=window.__eBoardStates||[];"
    "const s=states[states.length-1];"
    "return JSON.stringify({status:s&&s.chatStatus,"
    "run:s&&s.selectedRunId,detail:s&&s.detail.state});})()")
   (lambda (value)
     (when (stringp value)
       (setq e-board-visual-e2e--chat-state
             (json-read-from-string value)))))
  t)

(defun e-board-visual-e2e-chat-responding-p ()
  "Return whether an ordinary chat status reached the idle HUD."
  (and (equal (alist-get 'status e-board-visual-e2e--chat-state)
              "streaming")
       (null (alist-get 'run e-board-visual-e2e--chat-state))
       (equal (alist-get 'detail e-board-visual-e2e--chat-state)
              "empty")))

(defun e-board-visual-e2e-chat-ready-p ()
  "Return whether the settled chat status reached the idle HUD."
  (and (equal (alist-get 'status e-board-visual-e2e--chat-state)
              "done")
       (null (alist-get 'run e-board-visual-e2e--chat-state))
       (equal (alist-get 'detail e-board-visual-e2e--chat-state)
              "empty")))

(defun e-board-visual-e2e-send-state ()
  "Send the current coherent Board snapshot over the real WebKit bridge."
  (with-current-buffer e-board-visual-e2e--buffer
    (e-board-activity-visual--push-snapshot))
  t)

(defun e-board-visual-e2e-probe-delivery ()
  "Request one asynchronous observation of the WebKit snapshot."
  (setq e-board-visual-e2e--delivery nil)
  (xwidget-webkit-execute-script
   (e-board-visual-e2e--widget)
   (concat
    "(function(){const states=window.__eBoardStates||[];"
    "const s=states[states.length-1];"
    "return JSON.stringify({count:states.length,"
    "compact:s&&s.compact,"
    "run:s&&s.selectedRunId,"
    "required:s&&s.detail.requiredTasks[0].taskKey,"
    "participant:s&&s.detail.requiredTasks[0].participantId,"
    "optional:s&&s.detail.optionalTasks[0].taskKey,"
    "optionalParticipant:s&&s.detail.optionalTasks[0].participantId,"
    "canSteer:s&&s.detail.optionalTasks[0].controls.canSteer});})()")
   (lambda (value)
     (when (stringp value)
       (setq e-board-visual-e2e--delivery
             (json-read-from-string value)))))
  t)

(defun e-board-visual-e2e-delivered-p ()
  "Return whether WebKit received the selected run and both task groups."
  (let ((delivery e-board-visual-e2e--delivery))
    (and (> (or (alist-get 'count delivery) 0) 0)
         (eq (alist-get 'compact delivery) t)
         (equal (alist-get 'run delivery) "visual-run")
         (equal (alist-get 'required delivery) "calendar")
         (null (alist-get 'participant delivery))
         (equal (alist-get 'optional delivery) "slack")
         (stringp (alist-get 'optionalParticipant delivery))
         (eq (alist-get 'canSteer delivery) t))))

(defun e-board-visual-e2e-send-presentation-action (action)
  "Apply ACTION through the visual shell's semantic event handler.
The transparent focusless off-screen test frame can defer WebKit fetches; the
focused task-selection phase below checks the real inbound HTTP route."
  (with-current-buffer e-board-visual-e2e--buffer
    (let ((snapshot (e-board-activity-visual--snapshot)))
      (e-board-activity-visual--handle-ui-action
       `((action . ,action)
         (boardId . ,(alist-get 'boardId snapshot))
         (runSetEpoch . ,(alist-get 'runSetEpoch snapshot))))))
  t)

(defun e-board-visual-e2e-details-open-p ()
  "Return whether the explicit Details action opened the large view."
  (with-current-buffer e-board-visual-e2e--buffer
    (and (not e-board-activity-visual--compact)
         (eq (selected-frame) e-board-activity-shell--popup-frame)
         (> (frame-pixel-width e-board-activity-shell--popup-frame)
            e-board-activity-hud-width))))

(defun e-board-visual-e2e-hud-open-p ()
  "Return whether the explicit HUD action restored compact chat view."
  (with-current-buffer e-board-visual-e2e--buffer
    (and e-board-activity-visual--compact
         (eq (selected-frame) e-board-activity-shell--popup-parent)
         (frame-parameter e-board-activity-shell--popup-frame 'no-accept-focus)
         (>= (frame-pixel-width e-board-activity-shell--popup-frame)
             (- e-board-activity-hud-width
                (* 2 (frame-char-width e-board-activity-shell--popup-frame))))
         (<= (frame-pixel-width e-board-activity-shell--popup-frame)
             (+ e-board-activity-hud-width
                (frame-char-width e-board-activity-shell--popup-frame))))))

(defun e-board-visual-e2e-select-pending-task ()
  "Send one WebKit-to-Emacs task selection through the egui event route."
  (let* ((session
          (with-current-buffer e-board-visual-e2e--buffer
            e-board-activity-visual--egui-session))
         (snapshot
          (with-current-buffer e-board-visual-e2e--buffer
            (e-board-activity-visual--snapshot)))
         (detail (alist-get 'detail snapshot))
         (payload
          (json-encode
           `((action . "select-task")
             (boardId . ,(alist-get 'boardId snapshot))
             (runSetEpoch . ,(alist-get 'runSetEpoch snapshot))
             (generation . ,(alist-get 'generation detail))
             (revision . ,(alist-get 'revision detail))
             (runId . "visual-run") (taskKey . "calendar")
             (attempt . 0)))))
    (xwidget-webkit-execute-script
     (e-board-visual-e2e--widget)
     (format
      "fetch('/api/event?session=%s&action=ui-action&payload='+encodeURIComponent(%S))"
      (plist-get session :id) payload))
    t))

(defun e-board-visual-e2e-pending-task-selected-p ()
  "Return whether the WebKit action selected the pending task."
  (with-current-buffer e-board-visual-e2e--buffer
    (equal e-board-activity-visual--selected-task
           '(:run-task "visual-run" "calendar" 0))))

(defun e-board-visual-e2e-publish-update ()
  "Publish a Board notification and verify the selected task survives it."
  (let* ((fixture e-board-visual-e2e--fixture)
         (binding
          (e-chat-service-binding
           (plist-get fixture :harness) (plist-get fixture :session-id)))
         (target (e-chat-service-publication-target binding)))
    (e-board-activity-behavior-test--publish-fact
     target
     '(:version 1 :type task-attempt
       :idempotency-key "attempt:visual-run:calendar:0:running"
       :payload (:run-id "visual-run" :task-key "calendar"
                 :attempt 0 :status running)))
    (e-graphical-test-wait-until
     (lambda ()
       (with-current-buffer e-board-visual-e2e--buffer
         (let* ((page e-board-activity-visual--detail-page)
                (calendar
                 (cl-find "calendar" (plist-get page :tasks)
                          :key (lambda (task) (plist-get task :task-key))
                          :test #'equal)))
           (and (eq e-board-activity-visual--detail-state 'ready)
                (eq (plist-get calendar :state) 'running)
                (e-board-visual-e2e-pending-task-selected-p)))))
     5.0 "selected visual task after Board update")
    t))

(defun e-board-visual-e2e-open-native-fallback ()
  "Check that explicit native activity replaces this chat's visual HUD."
  (let* ((transcript (plist-get e-board-visual-e2e--fixture :transcript))
         (visual-popup
          (with-current-buffer e-board-visual-e2e--buffer
            e-board-activity-shell--popup-frame))
         (native
          (with-current-buffer transcript
            (cl-letf (((symbol-function
                         'e-board-activity-visual-unavailable-reason)
                        (lambda () "forced native fallback")))
              (funcall e-chat-surface--board-status-action)))))
    (unless (and (buffer-live-p native)
                 (not (buffer-live-p e-board-visual-e2e--buffer))
                 (not (frame-live-p visual-popup))
                 (with-current-buffer transcript e-chat--board-hud-dismissed)
                 (eq (window-buffer (selected-window)) native))
      (error "Native activity did not replace the automatic Board HUD: %S"
             (list :native native :frame-live (frame-live-p visual-popup)
                   :buffer-live (buffer-live-p e-board-visual-e2e--buffer)
                   :dismissed (with-current-buffer transcript
                                e-chat--board-hud-dismissed)
                   :selected (window-buffer (selected-window)))))
    (with-current-buffer native
      (e-board-activity-shell-dismiss))
    t))

(defun e-board-visual-e2e-finish ()
  "Release the disposable chat and its private SQL runtime."
  (when (buffer-live-p e-board-visual-e2e--buffer)
    (with-current-buffer e-board-visual-e2e--buffer
      (let ((parent e-board-activity-shell--popup-parent)
            (popup e-board-activity-shell--popup-frame))
        (e-board-activity-shell-dismiss)
        (unless (and (eq (selected-frame) parent)
                     (not (frame-visible-p popup))
                     (buffer-live-p e-board-visual-e2e--buffer))
          (error "Dismissing Board activity did not restore chat focus")))))
  (when e-board-visual-e2e--fixture
    (e-chat-behavior-test--cleanup
     e-board-visual-e2e--fixture
     (current-window-configuration)
     (cons (frame-width) (frame-height))))
  (when e-board-visual-e2e--child-store
    (ignore-errors
      (e-session-sqlite-store-close e-board-visual-e2e--child-store)))
  (when (and e-board-visual-e2e--child-directory
             (file-directory-p e-board-visual-e2e--child-directory))
    (delete-directory e-board-visual-e2e--child-directory t))
  (setq e-subagent-actions-default-live e-board-visual-e2e--original-live
        e-board-visual-e2e--fixture nil
        e-board-visual-e2e--child-store nil
        e-board-visual-e2e--child-directory nil)
  t)

(provide 'e-board-visual-e2e)
;;; e-board-visual-e2e.el ends here
