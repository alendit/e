;;; e-workspace-behavior-test.el --- Graphical persp workspace contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Visual workspace isolation, chat-window, and perspective-deletion contracts
;; using real persp-mode without loading Doom.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-actions)
(require 'e-backend)
(require 'e-emacs-capabilities)
(require 'e-workspaces)
(require 'e-chat-behavior-test)
(require 'e-persp-test-config)

(defvar e-workspace-behavior-test--buffers nil
  "Buffers owned by the dynamically active graphical workspace fixture.")

(defvar e-workspace-behavior-test--chat-fixture nil
  "Chat fixture owned by the dynamically active workspace test.")

(defun e-workspace-behavior-test--make-buffer (name text)
  "Create a fixture buffer NAME displaying TEXT."
  (let ((buffer (get-buffer-create name)))
    (push buffer e-workspace-behavior-test--buffers)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (set-buffer-modified-p nil)))
    buffer))

(defun e-workspace-behavior-test--switch (name)
  "Switch the selected frame to perspective NAME and settle redisplay."
  (persp-frame-switch name)
  (sit-for 0.01)
  (redisplay t)
  name)

(defun e-workspace-behavior-test--configure
    (name first-buffer &optional second-buffer)
  "Give perspective NAME one or two visible fixture buffers."
  (e-workspace-behavior-test--switch name)
  (delete-other-windows)
  (persp-add-buffer first-buffer (get-current-persp))
  (switch-to-buffer first-buffer)
  (when second-buffer
    (persp-add-buffer second-buffer (get-current-persp))
    (let ((window (split-window-right)))
      (set-window-buffer window second-buffer)
      (select-window window)))
  (redisplay t)
  (e-workspace-current))

(defun e-workspace-behavior-test--window-signature ()
  "Return the current perspective's observable normal-window signature."
  (let* ((root-edges (window-edges (frame-root-window)))
         (root-left (nth 0 root-edges))
         (root-top (nth 1 root-edges))
         (root-width (- (nth 2 root-edges) root-left))
         (root-height (- (nth 3 root-edges) root-top)))
    (list
     :workspace (safe-persp-name (get-current-persp))
     :selected (buffer-name (window-buffer (selected-window)))
     :windows
     (mapcar
      (lambda (window)
        (let ((edges (window-edges window)))
          (list
           :buffer (buffer-name (window-buffer window))
           ;; Frame size can settle asynchronously on NS.  Percent geometry
           ;; preserves the actual split topology without making one-column
           ;; frame rounding a workspace mutation.
           :geometry
           (list (round (* 100.0 (/ (float (- (nth 0 edges) root-left))
                                     root-width)))
                 (round (* 100.0 (/ (float (- (nth 1 edges) root-top))
                                     root-height)))
                 (round (* 100.0 (/ (float (- (nth 2 edges) root-left))
                                     root-width)))
                 (round (* 100.0 (/ (float (- (nth 3 edges) root-top))
                                     root-height))))
           :atom (and (window-atom-root window) t))))
      (sort (window-list nil 'nomini)
            (lambda (left right)
              (let ((left-edges (window-edges left))
                    (right-edges (window-edges right)))
                (or (< (nth 1 left-edges) (nth 1 right-edges))
                    (and (= (nth 1 left-edges) (nth 1 right-edges))
                         (< (car left-edges) (car right-edges)))))))))))

(defun e-workspace-behavior-test--cleanup
    (configuration frame-size save-directory)
  "Release the active fixture and restore frame state."
  (when-let ((stream (plist-get e-workspace-behavior-test--chat-fixture :stream)))
    (e-graphical-test-stream-cancel stream))
  (when-let ((transcript
              (plist-get e-workspace-behavior-test--chat-fixture :transcript)))
    (when (buffer-live-p transcript)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer transcript))))
  (when (bound-and-true-p persp-mode)
    (persp-mode -1)
    (sit-for 0.05))
  (dolist (buffer e-workspace-behavior-test--buffers)
    (when (buffer-live-p buffer)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer))))
  (when (window-configuration-p configuration)
    (set-window-configuration configuration))
  (set-frame-size (selected-frame) (car frame-size) (cdr frame-size))
  (when (file-directory-p save-directory)
    (delete-directory save-directory t))
  (redisplay t))

(defun e-workspace-behavior-test--with-basic-persp (function)
  "Run FUNCTION inside an isolated basic persp-mode graphical fixture."
  (let ((configuration (current-window-configuration))
        (frame-size (cons (frame-width) (frame-height)))
        (save-directory (make-temp-file "e-persp-graphical-" t))
        (e-workspace-behavior-test--buffers nil)
        (e-workspace-behavior-test--chat-fixture nil)
        (e-workspace-awareness-backend-priority '(persp single))
        (e-workspace-display-cross-workspace-policy 'switch))
    (unwind-protect
        (progn
          (e-persp-test-config-apply save-directory)
          (set-frame-size (selected-frame) 140 48)
          ;; NS applies frame resizing asynchronously.  Let the private test
          ;; frame reach its actual geometry before any fixture records a
          ;; window signature; otherwise a later resize can turn the same
          ;; split topology into a false geometry mismatch.
          (redisplay t)
          (sit-for 0.05)
          (redisplay t)
          (funcall function))
      (e-workspace-behavior-test--cleanup
       configuration frame-size save-directory))))

(ert-deftest e-workspace-behavior-test-agent-focus-preserves-workspace-configurations ()
  "Agent focus opens an updated buffer only in its owning perspective."
  (should (display-graphic-p))
  (e-workspace-behavior-test--with-basic-persp
   (lambda ()
     (let* ((owner-shell
             (e-workspace-behavior-test--make-buffer
              "*e workspace owner shell*" "owner workspace shell"))
            (target
             (e-workspace-behavior-test--make-buffer
              "*e workspace updated file*" "updated by the agent"))
            (other-left
             (e-workspace-behavior-test--make-buffer
              "*e workspace other left*" "unrelated workspace left"))
            (other-right
             (e-workspace-behavior-test--make-buffer
              "*e workspace other right*" "unrelated workspace right"))
            (owner-token
             (e-workspace-behavior-test--configure
              "e-owner" owner-shell))
            (owner-persp (get-current-persp)))
       (persp-add-buffer target owner-persp)
       (e-buffer-set-workspace target owner-token)
       (e-workspace-behavior-test--configure
        "e-other" other-left other-right)
       (let* ((other-persp (get-current-persp))
              (other-before
               (e-workspace-behavior-test--window-signature))
              (harness
               (e-harness-create
                :backend (e-backend-fake-create :items nil)
                :enabled-layer-ids nil)))
         (e-harness-activate-capability
          harness (e-workspace-awareness-capability-create))
         (e-graphical-test-capture-automatic-transition
          "workspace-agent-focus-owned-buffer"
          (lambda ()
            (e-actions-call
             'workspace-awareness :focus-buffer
             '(:buffer "*e workspace updated file*")
             (list :harness harness))))
         (should (equal (safe-persp-name (get-current-persp)) "e-owner"))
         (should (eq (window-buffer (selected-window)) target))
         (should (persp-contain-buffer-p target owner-persp))
         (should-not (persp-contain-buffer-p target other-persp))
         (e-workspace-behavior-test--switch "e-other")
         (e-graphical-test-wait-until
          (lambda ()
            (equal (e-workspace-behavior-test--window-signature)
                   other-before))
          2.0 "unrelated workspace window configuration restored"))))))

(ert-deftest e-workspace-behavior-test-chat-split-delete-survives-round-trip ()
  "Native atomic chat structure survives a perspective round trip."
  (should (display-graphic-p))
  (e-workspace-behavior-test--with-basic-persp
   (lambda ()
     (e-workspace-behavior-test--switch "e-chat-split")
     (setq e-workspace-behavior-test--chat-fixture
           (e-chat-behavior-test--open-surface))
     (let* ((fixture e-workspace-behavior-test--chat-fixture)
            (transcript (plist-get fixture :transcript))
            (split-buffer
             (e-workspace-behavior-test--make-buffer
              "*e workspace split target*" "ordinary split target"))
            (away-buffer
             (e-workspace-behavior-test--make-buffer
              "*e workspace split away*" "away workspace stays intact")))
       (persp-add-buffer transcript (get-current-persp))
       (persp-add-buffer (window-buffer (plist-get fixture :composer-window))
                         (get-current-persp))
       (should (eq (window-atom-root
                    (plist-get fixture :transcript-window))
                   (window-atom-root
                    (plist-get fixture :composer-window))))
       (should (eq (key-binding (kbd "C-x 3"))
                   #'e-chat-surface-split-window-right))
       (e-graphical-test-send-keys "C-x 3")
      (e-graphical-test-wait-until
       (lambda () (= (length (window-list nil 'nomini)) 3))
        2.0 "chat pair plus native right split")
       (e-graphical-test-send-keys "C-x o")
       (should-not
        (memq (selected-window)
              (list (plist-get fixture :transcript-window)
                    (plist-get fixture :composer-window))))
       (persp-add-buffer split-buffer (get-current-persp))
       (e-graphical-test-capture-automatic-transition
        "workspace-fill-chat-split"
        (lambda () (switch-to-buffer split-buffer)))
       (let ((chat-signature
              (e-workspace-behavior-test--window-signature)))
         (e-workspace-behavior-test--configure "e-split-away" away-buffer)
         (let ((away-signature
                (e-workspace-behavior-test--window-signature)))
           (e-graphical-test-capture-automatic-transition
            "workspace-restore-chat-split"
            (lambda ()
              (e-workspace-behavior-test--switch "e-chat-split")))
           (e-graphical-test-wait-until
            (lambda ()
              (and (= (length (window-list nil 'nomini)) 3)
                   (e-chat-behavior-test--surface-windows transcript)
                   (get-buffer-window split-buffer nil)))
            3.0 "restored chat split perspective")
           (should (equal (e-workspace-behavior-test--window-signature)
                          chat-signature))
           (e-workspace-behavior-test--switch "e-split-away")
           (should (equal (e-workspace-behavior-test--window-signature)
                          away-signature))
           (e-workspace-behavior-test--switch "e-chat-split")))
       (let* ((surface
               (e-chat-behavior-test--surface-windows transcript))
              (composer-window (cdr surface)))
         (should (eq (window-atom-root (car surface))
                     (window-atom-root composer-window)))
         (select-window composer-window)
         (should (eq (key-binding (kbd "C-x 0"))
                     #'e-persp-test-config-close-window-or-workspace))
         (should (equal (e-chat-surface-selected-chat-surface)
                        (cons transcript (car surface))))
         (e-graphical-test-send-keys "C-x 0")
         (should (= (length (window-list nil 'nomini)) 1))
         (should (eq (window-buffer (selected-window)) split-buffer))
         (should-not (get-buffer-window transcript nil)))))))

(ert-deftest e-workspace-behavior-test-chat-surface-restores-clean-target ()
  "Leaving a chat surface restores the target perspective exactly."
  (should (display-graphic-p))
  (e-workspace-behavior-test--with-basic-persp
   (lambda ()
     (let ((target
            (e-workspace-behavior-test--make-buffer
             "*e workspace clean target*" "target workspace remains whole")))
       (e-workspace-behavior-test--configure "e-clean-target" target)
       (let ((lower
              (split-window (selected-window)
                            (- e-chat-composer-window-min-height)
                            'below)))
         (set-window-buffer lower target)
         (select-window lower)
         ;; NS may apply the preceding frame resize one redisplay later.  Let
         ;; the target perspective settle before recording its exact geometry,
         ;; otherwise a transient 89/90 percent split becomes a false restore
         ;; failure when this test follows a chat surface test.
         (redisplay t)
         (sit-for 0.05)
         (redisplay t))
       (let ((target-signature
              (e-workspace-behavior-test--window-signature)))
         (e-workspace-behavior-test--switch "e-chat-source")
         (setq e-workspace-behavior-test--chat-fixture
               (e-chat-behavior-test--open-surface))
         (let* ((fixture e-workspace-behavior-test--chat-fixture)
                (transcript (plist-get fixture :transcript))
                (composer-window (plist-get fixture :composer-window))
                (composer (window-buffer composer-window))
                (source-persp (get-current-persp)))
           ;; Both presentation buffers and their native atom belong to the
           ;; same workspace.  Persp retains the atom through writable window
           ;; state without knowing that the surface belongs to e-chat.
           (should (persp-contain-buffer-p transcript source-persp))
           (should (persp-contain-buffer-p composer source-persp))
           (should (eq (selected-window) composer-window))
           (should
            (eq (window-atom-root
                 (plist-get fixture :transcript-window))
                (window-atom-root composer-window)))
           (e-graphical-test-capture-automatic-transition
            "workspace-leave-chat-surface"
            (lambda ()
              (e-workspace-behavior-test--switch "e-clean-target")))
           (e-graphical-test-wait-until
            (lambda ()
              (equal (e-workspace-behavior-test--window-signature)
                     target-signature))
            3.0 "clean target perspective restored")
           (should-not (get-buffer-window transcript nil))
           (should-not (get-buffer-window composer nil))))))))

(ert-deftest e-workspace-behavior-test-delete-chat-preserves-target-during-update ()
  "Deleting an updating chat perspective leaves the target perspective exact."
  (should (display-graphic-p))
  (e-workspace-behavior-test--with-basic-persp
   (lambda ()
     (let* ((target-left
             (e-workspace-behavior-test--make-buffer
              "*e workspace delete target left*" "target left is unchanged"))
            (target-right
             (e-workspace-behavior-test--make-buffer
              "*e workspace delete target right*" "target right is unchanged")))
       (e-workspace-behavior-test--configure
        "e-delete-target" target-left target-right)
       (let ((target-signature
              (e-workspace-behavior-test--window-signature)))
         (e-workspace-behavior-test--switch "e-delete-chat")
         (setq e-workspace-behavior-test--chat-fixture
               (e-chat-behavior-test--open-surface))
         (e-chat-behavior-test--submit
          e-workspace-behavior-test--chat-fixture "pending workspace update")
         (e-graphical-test-stream-emit
          (plist-get e-workspace-behavior-test--chat-fixture :stream)
          '(:type reasoning-delta :content "late deleted-workspace update")
          0.05)
         (e-graphical-test-capture-automatic-transition
          "workspace-delete-updating-chat"
          (lambda ()
            (e-persp-test-config-delete-current-workspace
             "e-delete-target")))
         (e-graphical-test-wait-until
          (lambda ()
            (null
             (e-graphical-test-stream-timers
              (plist-get e-workspace-behavior-test--chat-fixture :stream))))
          2.0 "provider update after chat perspective deletion")
         (should-not
          (e-graphical-test-stream-failure
           (plist-get e-workspace-behavior-test--chat-fixture :stream)))
         (should-not (persp-with-name-exists-p "e-delete-chat"))
         (should (equal (safe-persp-name (get-current-persp))
                        "e-delete-target"))
         (should (equal (e-workspace-behavior-test--window-signature)
                        target-signature))
         (should-not
          (get-buffer-window
           (plist-get e-workspace-behavior-test--chat-fixture :transcript)
           nil)))))))

(provide 'e-workspace-behavior-test)

;;; e-workspace-behavior-test.el ends here
