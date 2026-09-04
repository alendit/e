;;; e-runtime-store-recovery-behavior-test.el --- Graphical store recovery -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This is deliberately a small graphical composition witness.  The storage
;; work is real SQLite session/Board composition; only the provider stream is
;; controllable so the test remains network-free.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-board)
(require 'e-board-e2e-support)
(require 'e-chat)
(require 'e-chat-service)
(require 'e-harness)
(require 'e-session)
(require 'e-graphical-test-support)

(defun e-runtime-store-recovery-graphical--surface-windows (transcript)
  "Return the visible transcript and composer windows for TRANSCRIPT."
  (let ((transcript-window (get-buffer-window transcript nil))
        (composer-window
         (cl-find-if
          (lambda (window)
            (with-current-buffer (window-buffer window)
              (derived-mode-p 'e-chat-composer-mode)))
          (window-list nil 'nomini))))
    (and (window-live-p transcript-window)
         (window-live-p composer-window)
         (cons transcript-window composer-window))))

(defun e-runtime-store-recovery-graphical--prepare-frame ()
  "Reset and settle the isolated frame before composing the chat surface.

NS applies frame resizing asynchronously.  The native event boundaries here
keep a deferred resize from a preceding graphical fixture from rebuilding the
chat window tree after this test has already started its surface assertion."
  (delete-other-windows)
  (set-frame-size (selected-frame) 140 48)
  (sit-for 0.05)
  (redisplay t)
  (sit-for 0.05)
  (redisplay t))

(ert-deftest e-runtime-store-recovery-graphical-s92-durable-board-survives-catalog-ack-loss ()
  "A visible durable chat/Board surface remains usable after catalog recovery."
  (let* ((directory (make-temp-file "e-runtime-store-graphical-" t))
         (marker (make-temp-file "e-runtime-store-graphical-fault-"))
         (process-environment
          (cons "E_RUNTIME_STORE_TEST_FAULT=after-commit"
                (cons "E_RUNTIME_STORE_TEST_FAULT_OPERATION=catalog-put"
                      (cons (concat "E_RUNTIME_STORE_TEST_FAULT_ONCE_FILE=" marker)
                            process-environment))))
         sessions stream harness transcript)
    (delete-file marker)
    (unwind-protect
        (progn
          (e-board-e2e-reset-runtime)
          ;; The suite reuses one isolated graphical frame.  Reset its window
          ;; topology before asking the production chat surface to compose its
          ;; transcript/composer pair, so a prior test cannot hide either half.
          (e-runtime-store-recovery-graphical--prepare-frame)
          (setq sessions (e-session-sqlite-store-create directory)
                stream (e-graphical-test-stream-create)
                harness
                (e-harness-create
                 :backend (e-graphical-test-stream-backend stream)
                 :sessions sessions))
          (let* ((session
                  (e-chat-service-create-session
                   :harness harness :id "graphical-runtime-recovery"))
                 (session-id (plist-get session :id))
                 (binding (e-chat-service-binding harness session-id))
                 (registry-board (e-chat-service-binding-board binding))
                 (board (e-board-registry-board-source-board
                         registry-board))
                 (_participant
                  (e-board-registry-add-participant
                   registry-board :id "recovery-member"
                   :subscription-id "recovery-address"
                   :principal
                   (e-board-registry-client-principal
                    (e-chat-service-binding-client binding))))
                 (publication
                 (e-board-post-input
                   board :id "recovery-input" :to "recovery-member" :content "route"
                   :requester-actor (e-board-registry-board-principal registry-board)
                   :source-input-key '(graphical-recovery 1 1)))
                 (pickup-id
                  (progn
                    ;; The durable Board's production scheduler is timer-based;
                    ;; establish its ready head before the *second* timer is
                    ;; deliberately queued behind the catalog recovery.
                    (while (e-board-input-classifications board)
                      (e-board-drain-input-classifications board))
                    (let ((id (car (e-board-publication-pickup-ids publication))))
                      (unless id
                        (ert-fail
                         (format "No durable pickup: state=%S reason=%S"
                                 (e-board-message-routing-state
                                  (e-board-publication-message publication))
                                 (e-board-message-unrouted-reason
                                  (e-board-publication-message publication)))))
                      id)))
                 claimed)
            (setq transcript (e-chat-open-session harness session-id t))
            (e-graphical-test-wait-until
             (lambda ()
               (e-runtime-store-recovery-graphical--surface-windows transcript))
             2.0 "visible durable chat surface")
            ;; The timer queues the production Board storage transition while
            ;; the catalog-put receipt is being resolved after worker loss.
            (run-at-time
             0 nil
             (lambda ()
               (setq claimed
                     (condition-case err
                         (e-board-pickup-start-delivery board pickup-id)
                       (error err)))))
            (should (> (plist-get
                        (e-session-storage-sqlite-write-catalog
                         sessions '((:id "graphical-recovery")))
                        :revision)
                       0))
            (should (equal (e-session-storage-sqlite-read-catalog sessions)
                           '((:id "graphical-recovery"))))
            (e-graphical-test-wait-until
             (lambda () claimed)
             2.0 "durable Board delivery after catalog recovery")
            (unless (e-board-pickup-p claimed)
              (ert-fail (format "Board transition failed: %S" claimed)))
            (should (eq (e-board-pickup-state (e-board-pickup board pickup-id))
                        'delivering))
            (should (file-exists-p marker))
            (should-not
             (plist-get
              (e-runtime-store-status (e-session-storage-runtime-store sessions))
              :unavailable))
            (let ((windows
                   (e-runtime-store-recovery-graphical--surface-windows transcript)))
              (should windows)
              (select-window (cdr windows))
              (e-graphical-test-type-text "surface remains usable")
              (with-current-buffer (window-buffer (cdr windows))
                (should (string-match-p "surface remains usable" (buffer-string))))
              (when (e-graphical-test-screenshot-enabled-p)
                (e-graphical-test-capture-state "runtime-store-recovery-visible")))))
      (when (buffer-live-p transcript) (kill-buffer transcript))
      (when (e-graphical-test-stream-p stream)
        (e-graphical-test-stream-cancel stream))
      (when sessions (ignore-errors (e-session-sqlite-store-close sessions)))
      (when (file-exists-p marker) (delete-file marker))
      (delete-directory directory t))))

(provide 'e-runtime-store-recovery-behavior-test)

;;; e-runtime-store-recovery-behavior-test.el ends here
