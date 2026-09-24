;;; e-board-reasoning-behavior-test.el --- Graphical Board reasoning composition -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-board-activity-shell)
(require 'e-board-observation)
(require 'e-chat-behavior-test)
(require 'e-chat-service)
(require 'e-graphical-test-support)
(require 'e-session-sqlite)
(require 'e-work)

(ert-deftest e-board-reasoning-behavior-test-held-append-publishes-safe-detail ()
  "Live chat stays responsive while the committed Board summary is delayed."
  (should (display-graphic-p))
  (let* ((configuration (current-window-configuration))
         (frame-size (cons (frame-width) (frame-height)))
         (stall-directory (make-temp-file "e-board-reasoning-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY="
                        stall-directory)
                process-environment))
         (hold (expand-file-name "session-append.hold" stall-directory))
         (ready (expand-file-name "session-append.ready" stall-directory))
         (release (expand-file-name "session-append.release" stall-directory))
         fixture restart-fixture board-buffer detail-buffer
         restart-board-buffer restart-detail-buffer)
    (unwind-protect
        (progn
          (setq fixture (e-chat-behavior-test--open-surface))
          (e-chat-behavior-test--submit fixture "Board reasoning composition")
          (e-chat-behavior-test--emit
           fixture
           '(:type reasoning-delta :stream-kind summary
             :content "first live summary line")
           "first live summary line")
          (e-chat-behavior-test--emit
           fixture
           '(:type reasoning-delta :stream-kind summary
             :content "second live summary line")
           "second live summary line")
          ;; The provider may also send private/raw material.  It must not
          ;; enter the ordinary transcript or the later Board projection.
          (e-graphical-test-stream-emit
           (plist-get fixture :stream)
           '(:type reasoning-raw-delta :stream-kind raw
             :content "PRIVATE_RAW_SHOULD_NOT_APPEAR")
           0.01)
          (e-graphical-test-stream-emit
           (plist-get fixture :stream)
           '(:type provider-replay-item
             :provider-id openai
             :item (:type "reasoning"
                    :encrypted_content "PRIVATE_ENCRYPTED_SHOULD_NOT_APPEAR"))
           0.01)
          ;; Hold only the request-boundary append.  The selected chat has
          ;; already received both live fragments and remains editable.
          (write-region "hold" nil hold nil 'silent)
          (e-graphical-test-stream-emit
           (plist-get fixture :stream)
           '(:type assistant-message :content "answer after reasoning") 0.01)
          (e-graphical-test-stream-finish (plist-get fixture :stream) 0.03)
          (e-graphical-test-wait-until
           (lambda () (file-exists-p ready))
           2.0 "held session activity append")
          (with-current-buffer (plist-get fixture :transcript)
            (should (string-match-p "first live summary line" (buffer-string)))
            (should (string-match-p "second live summary line" (buffer-string)))
            (should-not (string-match-p "PRIVATE_RAW_SHOULD_NOT_APPEAR"
                                        (buffer-string)))
            (should-not (string-match-p "PRIVATE_ENCRYPTED_SHOULD_NOT_APPEAR"
                                        (buffer-string))))
          ;; Release persistence and wait for the Board bridge to publish its
          ;; one idempotent sanitized row.
          (write-region "release" nil release nil 'silent)
          (e-graphical-test-wait-until
           (lambda ()
             (with-current-buffer (plist-get fixture :transcript)
               (and (string-match-p "answer after reasoning" (buffer-string))
                    (equal (e-chat-surface-status) "done"))))
           4.0 "settled chat snapshot")
          (let* ((harness (plist-get fixture :harness))
                 (session-id (plist-get fixture :session-id))
                 (binding (e-chat-service-binding harness session-id))
                 (target (e-chat-service-publication-target binding)))
            (setq board-buffer
                  (e-board-activity-list-buffer :target target :live nil))
            (e-graphical-test-wait-until
             (lambda ()
               (and (buffer-live-p board-buffer)
                    (with-current-buffer board-buffer
                      (and (string-match-p "first live summary line"
                                           (buffer-string))
                           (string-match-p "second live summary line"
                                           (buffer-string))))))
             4.0 "Board summary preview")
            (with-current-buffer board-buffer
              (goto-char (point-min))
              (while (and (not (tabulated-list-get-id))
                          (not (eobp)))
                (forward-line 1)))
            (e-board-activity-shell-show-summary)
            (setq detail-buffer
                  (get-buffer e-board-activity-shell-detail-buffer-name))
            (e-graphical-test-wait-until
             (lambda ()
               (and (buffer-live-p detail-buffer)
                    (with-current-buffer detail-buffer
                      (string-match-p "first live summary line"
                                      (buffer-string)))))
             4.0 "Board summary detail")
            (with-current-buffer detail-buffer
              (should (string-match-p "second live summary line"
                                       (buffer-string)))
              (should-not (string-match-p "PRIVATE_RAW_SHOULD_NOT_APPEAR"
                                          (buffer-string)))
              (should-not (string-match-p "PRIVATE_ENCRYPTED_SHOULD_NOT_APPEAR"
                                          (buffer-string))))
            ;; Replace the process-local session owner and reopen the same
            ;; durable SQLite directory.  The new binding derives the same
            ;; Board identity from the persisted session association.
            (let* ((directory (plist-get fixture :store-directory))
                   (old-store (plist-get fixture :store))
                   (old-transcript (plist-get fixture :transcript))
                   (restart-stream (e-graphical-test-stream-create))
                   (restart-store nil)
                   (restart-harness nil)
                   (restart-transcript nil))
              (when (buffer-live-p board-buffer)
                (kill-buffer board-buffer))
              (when (buffer-live-p detail-buffer)
                (kill-buffer detail-buffer))
              (e-graphical-test-stream-cancel
               (plist-get fixture :stream))
              (when (buffer-live-p old-transcript)
                (kill-buffer old-transcript))
              (ignore-errors
                (e-runtime-store-shutdown
                 (e-session-storage-runtime-store old-store)))
              (setq fixture nil)
              (setq restart-store (e-session-sqlite-store-create directory))
              (e-session-enable restart-store)
              (setq restart-harness
                    (e-harness-create
                     :backend (e-graphical-test-stream-backend restart-stream)
                     :sessions restart-store
                     :default-options
                     '(:model "gpt-5.6-sol" :reasoning-effort "high")))
              (setq restart-transcript
                    (e-chat-open :harness restart-harness
                                 :session-id session-id))
              (e-chat-surface-pop-to-buffer restart-transcript)
              (setq restart-fixture
                    (list :stream restart-stream
                          :harness restart-harness
                          :session-id session-id
                          :transcript restart-transcript
                          :store restart-store
                          :store-directory directory))
              (e-graphical-test-wait-until
               (lambda ()
                 (and (buffer-live-p restart-transcript)
                      (e-chat-service-binding restart-harness session-id)))
               5.0 "reopened session Board binding")
              (let* ((restart-binding
                      (e-chat-service-binding restart-harness session-id))
                     (restart-target
                      (e-chat-service-publication-target restart-binding)))
                (setq restart-board-buffer
                      (e-board-activity-list-buffer
                       :target restart-target :live nil))
                (e-graphical-test-wait-until
                 (lambda ()
                   (and (buffer-live-p restart-board-buffer)
                        (with-current-buffer restart-board-buffer
                          (and (string-match-p "first live summary line"
                                               (buffer-string))
                               (string-match-p "second live summary line"
                                               (buffer-string))
                               (= (count-matches "first live summary line")
                                  1)))))
                 5.0 "reopened Board summary preview")
                (with-current-buffer restart-board-buffer
                  (goto-char (point-min))
                  (while (and (not (tabulated-list-get-id))
                              (not (eobp)))
                    (forward-line 1)))
                (e-board-activity-shell-show-summary)
                (setq restart-detail-buffer
                      (get-buffer e-board-activity-shell-detail-buffer-name))
                (e-graphical-test-wait-until
                 (lambda ()
                   (and (buffer-live-p restart-detail-buffer)
                        (with-current-buffer restart-detail-buffer
                          (and (string-match-p "first live summary line"
                                               (buffer-string))
                               (not (string-match-p
                                     "PRIVATE_RAW_SHOULD_NOT_APPEAR"
                                     (buffer-string)))
                               (not (string-match-p
                                     "PRIVATE_ENCRYPTED_SHOULD_NOT_APPEAR"
                                     (buffer-string)))))))
                 5.0 "reopened Board summary detail"))))
          (e-chat-behavior-test--cleanup
           restart-fixture configuration frame-size)
          (setq restart-fixture nil)
          (e-chat-behavior-test--capture "board-reasoning-detail")
          )
      (write-region "release" nil release nil 'silent)
      (when (and fixture (buffer-live-p (plist-get fixture :transcript)))
        (ignore-errors
          (e-chat-behavior-test--cleanup fixture configuration frame-size)))
      (when (and restart-fixture
                 (buffer-live-p (plist-get restart-fixture :transcript)))
        (ignore-errors
          (e-chat-behavior-test--cleanup
           restart-fixture configuration frame-size)))
      (delete-directory stall-directory t))))

(provide 'e-board-reasoning-behavior-test)

;;; e-board-reasoning-behavior-test.el ends here
