;;; e-chat-composer-mechanism-test.el --- Composer owner mechanism tests -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests exercise composer-owned private mechanisms through the composed
;; shell fixture.  The public composed behavior remains in
;; `e-chat-presentation-integration-test.el'; standalone owner contracts live
;; in `e-chat-composer-test.el'.

;;; Code:

(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)


(ert-deftest e-chat-test-reload-preserves-composed-surface-pair ()
  "Reload preserves the atomic pair and its composer draft."
  (let* ((configuration (current-window-configuration))
         (buffer (e-chat-test--buffer nil "chat-reload-composed-surface"))
         transcript-window
         composer)
    (unwind-protect
        (progn
          (delete-other-windows)
          (set-window-buffer (selected-window) buffer)
          (setq transcript-window (get-buffer-window buffer))
          (with-current-buffer buffer
            (setq composer
                  (window-buffer
                   (e-chat-surface-display-composer transcript-window))))
          (with-current-buffer buffer
            (with-current-buffer composer
              (goto-char (point-max))
              (insert "draft survives reload"))
            (should (= (length
                        (get-buffer-window-list composer nil t))
                       1))
            ;; Reinitializing `e-chat-mode' leaves the atomic window pair and
            ;; composer buffer alive; no window cache needs reconstruction.
            (e-chat-attach-buffer
             buffer e-chat-harness e-chat-session-id
             e-chat-harness-instance-id)
            (e-chat-surface-after-display-buffer buffer)
            (should (eq (e-chat-surface-composer-buffer buffer) composer))
            (should (eq (e-chat-surface-composer-window transcript-window)
                        (get-buffer-window composer t)))
            (should (eq (e-chat-surface-transcript-buffer composer)
                        buffer))
            (e-chat-surface-set-status "reload")
            ;; Host mode-line packages may temporarily replace the transcript's
            ;; display form during reload.  The composer projects semantic
            ;; status and must never forward an evaluable form recursively.
            (setq-local mode-name '(:eval (format-mode-line mode-name)))
            (with-current-buffer composer
              (should (stringp (format-mode-line mode-name))))
            (should (= (length
                        (get-buffer-window-list composer nil t))
                       1))
            (with-current-buffer composer
              (should (equal (e-chat-composer-text)
                             "draft survives reload")))))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-queued-prompts-render-in-composer ()
  "Queued prompts appear above editable input in the composer buffer."
  (let ((buffer (e-chat-test--buffer nil "chat-queue-render")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (cl-letf (((symbol-function 'e-chat-service-queued-inputs)
                     (lambda (&rest _)
                       '((:prompt "second line\ncontinued")
                         (:prompt "third")))))
            (e-chat-composer-insert-queued-prompts)
            (let* ((content (buffer-string))
                   (queue-pos (and (markerp e-chat-composer--queue-start-marker)
                                   (marker-position
                                    e-chat-composer--queue-start-marker)))
                   (composer-pos (e-chat-composer-start-position)))
              (should queue-pos)
              (should composer-pos)
              (should (< queue-pos composer-pos))
              (should (string-match-p "Queued prompts" content))
              (should (string-match-p "1\\. second line continued" content))
              (should (string-match-p "2\\. third" content)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-slash-only-triggers-at-leading-position ()
  "Composer / expands prompts only as the first non-whitespace input."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-slash-leading"))
          (with-current-buffer (e-chat-test--composer buffer)
            (let ((prompt (e-prompt-spec-create
                           :name "review"
                           :description "Review code."
                           :parameters nil
                           :template "Review now.")))
              (cl-letf (((symbol-function 'e-chat-composer--prompt-candidates)
                         (lambda ()
                           (list (list :label "review" :prompt prompt)))))
                (let ((unread-command-events (list ?\r)))
                  (e-chat-composer-slash))
                (should (equal (e-chat-composer-text) "Review now.")))
              (delete-region (e-chat-composer-start-position) (point-max))
              (insert "please ")
              (cl-letf (((symbol-function 'e-chat-composer--prompt-candidates)
                         (lambda ()
                           (error "non-leading / must not collect prompts"))))
                (let ((last-command-event ?/))
                  (e-chat-composer-slash)))
              (should (equal (e-chat-composer-text) "please /")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-bang-inserts-command-output-reference ()
  "Leading ! inserts pending output, then completes the context reference."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-bang"))
          (with-current-buffer (e-chat-test--composer buffer)
            (let (done)
              (cl-letf (((symbol-function 'read-shell-command)
                         (lambda (&rest _args) "printf hi"))
                        ((symbol-function 'e-chat-composer--run-shell-command-start)
                         (lambda (command directory &rest args)
                           (should (equal command "printf hi"))
                           (should (file-directory-p directory))
                           (setq done (plist-get args :on-done))
                           (e-tools-request-create :cancel (lambda () t)))))
                (e-chat-composer-bang))
              (let* ((document (e-chat-composer-document))
                     (references (plist-get document :references)))
                (should (= (length references) 1))
                (should (e-chat-composer--pending-context-reference-p
                         (car references)))
                (should (string-match-p
                         (regexp-quote
                          "<reference id=\"ref-1\" label=\"$ printf hi (running)\">")
                         (plist-get document :text))))
              (funcall done (list :output "hi\n" :exit 0)))
            (let* ((document (e-chat-composer-document))
                   (references (plist-get document :references))
                   (submission (plist-get (e-chat-composer-submission)
                                          :prompt)))
              (should (= (length references) 1))
              (should-not (e-chat-composer--pending-context-reference-p
                           (car references)))
              (should (string-match-p
                       (regexp-quote "<reference id=\"ref-1\" label=\"$ printf hi (exit 0)\">")
                       (plist-get document :text)))
              (should (string-match-p (regexp-quote "$ printf hi") submission))
              (should (string-match-p (regexp-quote "Status: exit 0") submission))
              (should (string-match-p (regexp-quote "hi") submission)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-bang-captures-nonzero-timeout-and-truncation ()
  "Command references preserve failure, timeout, and truncation state as text."
  (let* ((e-chat-command-output-timeout 7)
         (reference (e-chat-composer--command-output-reference
                     "broken"
                     (list :output "stderr\n"
                           :exit 2
                           :truncated t)))
         (timeout-reference (e-chat-composer--command-output-reference
                             "slow"
                             (list :output "partial\n"
                                   :timed-out t))))
    (should (string-match-p (regexp-quote "$ broken (exit 2)")
                            (plist-get reference :label)))
    (should (string-match-p (regexp-quote "Status: exit 2")
                            (plist-get reference :text)))
    (should (string-match-p (regexp-quote "Output was truncated.")
                            (plist-get reference :text)))
    (should (string-match-p (regexp-quote "stderr")
                            (plist-get reference :text)))
    (should (string-match-p (regexp-quote "$ slow (timed out after 7s)")
                            (plist-get timeout-reference :label)))
    (should (string-match-p (regexp-quote "Status: timed out after 7s")
                            (plist-get timeout-reference :text)))))





(ert-deftest e-chat-test-composer-bang-truncates-real-command-output ()
  "Shell command capture caps oversized output with a visible marker."
  (let ((e-chat-command-output-max-bytes 5)
        (e-chat-command-output-timeout 5))
    (let ((result (e-chat-composer--run-shell-command "printf 0123456789" temporary-file-directory)))
      (should (equal (plist-get result :exit) 0))
      (should (plist-get result :truncated))
      (should (string-prefix-p "01234" (plist-get result :output)))
      (should (string-match-p (regexp-quote "[Command output truncated]")
                              (plist-get result :output))))))





(ert-deftest e-chat-test-sync-command-output-rejects-hot-path ()
  "The synchronous command-output helper fails before starting a shell command."
  (let (started)
    (cl-letf (((symbol-function 'e-chat-composer--run-shell-command-start)
               (lambda (&rest _args)
                 (setq started t)
                 (error "shell command should not start"))))
      (let ((err (should-error
                  (e-request-with-hot-path 'chat-sync-command
                    (e-chat-composer--run-shell-command
                     "printf done"
                     temporary-file-directory))
                  :type 'e-request-blocking-call-in-hot-path)))
        (should (equal (cdr err)
                       '(e-chat-composer--run-shell-command chat-sync-command))))
      (should-not started))))





(ert-deftest e-chat-test-composer-prefix-cancel-keeps-literal-character ()
  "Cancelling a prefix popup leaves the typed prefix in the composer."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-cancel"))
          (with-current-buffer (e-chat-test--composer buffer)
            (cl-letf (((symbol-function 'read-shell-command)
                       (lambda (&rest _args) (signal 'quit nil))))
              (e-chat-composer-bang))
            (insert " ")
            (let ((unread-command-events (list ?\C-g)))
              (cl-letf (((symbol-function 'e-chat-composer--project-file-candidates)
                       (lambda () (list (list :label "a.txt" :path "/tmp/a.txt")))))
                (e-chat-composer-at)))
            (insert " ")
            (let ((unread-command-events (list ?\C-g)))
              (cl-letf (((symbol-function 'e-chat-composer--prompt-candidates)
                       (lambda () (list (list :label "review" :prompt 'prompt)))))
                (let ((last-command-event ?/))
                  (e-chat-composer-slash))))
            (should (equal (e-chat-composer-text) "! @ /"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-empty-prefix-candidates-keep-literal-character ()
  "Empty file and prompt candidate sets leave the typed prefix literal."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-empty"))
          (with-current-buffer (e-chat-test--composer buffer)
            (cl-letf (((symbol-function 'e-chat-composer--project-file-candidates)
                       (lambda () nil))
                      ((symbol-function 'e-chat-composer--prompt-candidates)
                       (lambda () nil)))
              (e-chat-composer-at)
              (insert " ")
              (let ((last-command-event ?/))
                (e-chat-composer-slash)))
            (should (equal (e-chat-composer-text) "@ /"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-at-lists-git-project-files ()
  "Project file candidates come from git and honor exclude-standard rules."
  (let ((directory (make-temp-file "e-chat-prefix-files-" t)))
    (unwind-protect
        (progn
          (process-file "git" nil nil nil "-C" directory "init")
          (with-temp-file (expand-file-name ".gitignore" directory)
            (insert "ignored.txt\n"))
          (with-temp-file (expand-file-name "keep.txt" directory)
            (insert "keep\n"))
          (make-directory (expand-file-name "sub" directory))
          (with-temp-file (expand-file-name "sub/nested.el" directory)
            (insert "(message \"nested\")\n"))
          (with-temp-file (expand-file-name "ignored.txt" directory)
            (insert "ignored\n"))
          (cl-letf (((symbol-function 'e-chat-composer--workspace-roots)
                     (lambda () (list directory))))
            (let ((labels (mapcar (lambda (candidate)
                                    (plist-get candidate :label))
                                  (e-chat-composer--project-file-candidates-sync))))
              (should (member "keep.txt" labels))
              (should (member "sub/nested.el" labels))
              (should-not (member "ignored.txt" labels)))))
      (delete-directory directory t))))





(ert-deftest e-chat-test-composer-at-does-not-fallback-in-ignored-only-git-root ()
  "Git-backed candidate listing does not expose ignored files by fallback scan."
  (let ((directory (make-temp-file "e-chat-prefix-ignored-" t)))
    (unwind-protect
        (progn
          (process-file "git" nil nil nil "-C" directory "init")
          (with-temp-file (expand-file-name ".gitignore" directory)
            (insert "*\n"))
          (with-temp-file (expand-file-name "ignored.txt" directory)
            (insert "ignored\n"))
          (cl-letf (((symbol-function 'e-chat-composer--workspace-roots)
                     (lambda () (list directory))))
            (let ((labels (mapcar (lambda (candidate)
                                    (plist-get candidate :label))
                                  (e-chat-composer--project-file-candidates-sync))))
              (should-not (member "ignored.txt" labels)))))
      (delete-directory directory t))))





(ert-deftest e-chat-test-composer-at-skips-missing-workspace-roots ()
  "Project file completion ignores stale workspace roots and keeps live roots."
  (let ((directory (make-temp-file "e-chat-prefix-live-root-" t))
        (missing (expand-file-name "missing-root" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "keep.txt" directory)
            (insert "keep\n"))
          (when (file-exists-p missing)
            (delete-directory missing t))
          (cl-letf (((symbol-function 'e-chat-composer--workspace-roots)
                     (lambda () (list directory missing))))
            (let ((labels (mapcar (lambda (candidate)
                                    (plist-get candidate :label))
                                  (e-chat-composer--project-file-candidates-sync))))
              (should (member "keep.txt" labels)))))
      (delete-directory directory t))))





(ert-deftest e-chat-test-composer-at-uses-fd-before-recursive-fallback ()
  "Non-git project file completion prefers fd before recursive Lisp fallback."
  (let ((directory (file-name-as-directory
                    (make-temp-file "e-chat-prefix-fd-root-" t))))
    (unwind-protect
        (cl-letf (((symbol-function 'e-chat-composer--workspace-roots)
                   (lambda () (list directory)))
                  ((symbol-function 'e-chat-composer--git-file-candidates)
                   (lambda (_root _limit) nil))
                  ((symbol-function 'e-chat-composer--fd-file-candidates)
                   (lambda (root limit)
                     (should (equal root directory))
                     (should (= limit e-chat-project-file-candidate-limit))
                     (list (expand-file-name "from-fd.txt" root))))
                  ((symbol-function 'e-chat-composer--fallback-file-candidates)
                   (lambda (&rest _args)
                     (error "recursive fallback should not run when fd succeeds"))))
          (let ((labels (mapcar (lambda (candidate)
                                  (plist-get candidate :label))
                                (e-chat-composer--project-file-candidates-sync))))
            (should (equal labels '("from-fd.txt")))))
      (delete-directory directory t))))





(ert-deftest e-chat-test-sync-project-file-candidates-rejects-hot-path ()
  "The synchronous composer file scanner fails before process work in hot paths."
  (let (started)
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _args)
                 (setq started t)
                 (error "process-file should not run"))))
      (let ((err (should-error
                  (e-request-with-hot-path 'chat-file-candidates
                    (e-chat-composer--project-file-candidates-sync))
                  :type 'e-request-blocking-call-in-hot-path)))
        (should (equal (cdr err)
                       '(e-chat-composer--project-file-candidates-sync
                         chat-file-candidates))))
      (should-not started))))





(ert-deftest e-chat-test-fd-file-candidates-invokes-fd-for-files ()
  "fd candidate collection asks fd for hidden, non-.git regular files."
  (let ((directory (file-name-as-directory
                    (make-temp-file "e-chat-prefix-fd-command-" t)))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'e-chat-composer--fd-executable)
                   (lambda () "fd"))
                  ((symbol-function 'process-file)
                   (lambda (program _infile _destination _display &rest args)
                     (setq calls (cons program args))
                     (insert ".hidden\n")
                     (insert "visible.txt\n")
                     0)))
          (should (equal (e-chat-composer--fd-file-candidates directory 1)
                         (list (expand-file-name ".hidden" directory))))
          (should (equal (car calls) "fd"))
          (should (member "--type" (cdr calls)))
          (should (member "file" (cdr calls)))
          (should (member "--hidden" (cdr calls)))
          (should (member "--exclude" (cdr calls)))
          (should (member ".git" (cdr calls)))
          (should (member "--base-directory" (cdr calls)))
          (should (member directory (cdr calls))))
      (delete-directory directory t))))





(ert-deftest e-chat-test-composer-at-file-candidates-refresh-asynchronously ()
  "Public composer file candidates return cached snapshots and refresh later."
  (let ((buffer (e-chat-test--buffer nil "chat-prefix-files-async"))
        (calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat-composer--project-file-candidates-sync)
                     (lambda ()
                       (setq calls (1+ calls))
                       (list (list :label "async.txt"
                                   :path "/tmp/async.txt"
                                   :root "/tmp/")))))
            (should-not (e-chat-composer--project-file-candidates))
            (should (= calls 0))
            (should (e-chat-test--wait-until
                     (lambda ()
                       (e-chat-composer--project-file-candidate-cache-hit-p
                        (e-chat-composer--project-file-candidate-cache-key)))))
            (should (= calls 1))
            (should (equal (mapcar (lambda (candidate)
                                     (plist-get candidate :label))
                                   (e-chat-composer--project-file-candidates))
                           '("async.txt")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-at-shows-loading-file-candidates ()
  "Composer @ exposes a loading row while file candidates refresh."
  (let ((buffer (e-chat-test--buffer nil "chat-prefix-files-loading"))
        (calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat-composer--project-file-candidates-sync)
                     (lambda ()
                       (setq calls (1+ calls))
                       nil)))
            (let ((labels (mapcar (lambda (candidate)
                                    (plist-get candidate :label))
                                  (e-chat-composer--at-candidates))))
              (should (member "files: loading..." labels))
              (should (= calls 0)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-at-file-candidate-refresh-is-latest-only ()
  "Stale composer file candidate refreshes cannot replace newer roots."
  (let ((buffer (e-chat-test--buffer nil "chat-prefix-files-latest"))
        (root-a (file-name-as-directory
                 (make-temp-file "e-chat-prefix-root-a-" t)))
        (root-b (file-name-as-directory
                 (make-temp-file "e-chat-prefix-root-b-" t)))
        roots)
    (unwind-protect
        (with-current-buffer buffer
          (setq roots (list root-a))
          (cl-letf (((symbol-function 'e-chat-composer--workspace-roots)
                     (lambda () roots))
                    ((symbol-function 'e-chat-composer--project-file-candidates-sync)
                     (lambda ()
                       (list (list :label (file-name-nondirectory
                                           (directory-file-name
                                            (car roots)))
                                   :path (expand-file-name "file.txt"
                                                           (car roots))
                                   :root (car roots))))))
            (should-not (e-chat-composer--project-file-candidates))
            (setq roots (list root-b))
            (should-not (e-chat-composer--project-file-candidates))
            (should (e-chat-test--wait-until
                     (lambda ()
                       (let ((candidate
                              (car (e-chat-composer--project-file-candidates))))
                         (equal (plist-get candidate :root) root-b)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root-a t)
      (delete-directory root-b t))))





(ert-deftest e-chat-test-composer-at-file-candidate-refresh-cancels-on-kill ()
  "Killing a chat buffer cancels pending composer file candidate refresh."
  (let ((buffer (e-chat-test--buffer nil "chat-prefix-files-kill"))
        (calls 0))
    (with-current-buffer buffer
      (cl-letf (((symbol-function 'e-chat-composer--project-file-candidates-sync)
                 (lambda ()
                   (setq calls (1+ calls))
                   nil)))
        (e-chat-composer--project-file-candidates)
        (kill-buffer buffer)
        (accept-process-output nil 0.05)
        (should (= calls 0))))))





(ert-deftest e-chat-test-inline-completion-matches-case-insensitive-fuzzy ()
  "Inline completion filters labels by case-insensitive ordered characters."
  (let ((candidates '((:label ".gitignore")
                      (:label ".github/workflows/test.yml")
                      (:label "src/example.el"))))
    (should (equal (mapcar (lambda (candidate)
                             (plist-get candidate :label))
                           (e-chat-composer--inline-completion-matches candidates "GIT"))
                   '(".gitignore" ".github/workflows/test.yml")))
    (should (equal (mapcar (lambda (candidate)
                             (plist-get candidate :label))
                           (e-chat-composer--inline-completion-matches candidates "sre"))
                   '("src/example.el")))))





(ert-deftest e-chat-test-inline-completion-render-keeps-prompt-on-current-line ()
  "Inline completion renders the prompt at point instead of below the composer."
  (let ((text (e-chat-composer--inline-completion-render
               "@ file: "
               '((:label ".gitignore")
                 (:label ".github/workflows/test.yml"))
               0
               "git")))
    (should (string-prefix-p "@ file: git\n> .gitignore" text))
    (should-not (string-prefix-p "\n" text))))





(ert-deftest e-chat-test-inline-completion-del-deletes-filter-character ()
  "DEL removes the previous inline-completion filter character."
  (let ((keys (list ?A ?Z ?\177 ?B ?\r ?\C-g)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-key)
                 (lambda ()
                   (pop keys))))
        (should (equal (e-chat-composer--inline-completion-select
                        "@ file: "
                        '((:label "ab.txt")
                          (:label "ac.txt")))
                       '(:label "ab.txt")))))))





(ert-deftest e-chat-test-composer-at-lists-files-resources-and-capabilities ()
  "Composer @ candidates include files, active resources, and capabilities."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-at-candidates"))
          (with-current-buffer buffer
            (let ((capability
                   (e-capability-create
                    :id 'reference-capability
                    :name "Reference Capability"
                    :instructions "Use references."
                    :resources
                    (list (lambda (store capability)
                            (e-store-register
                             store
                             (e-capability-id capability)
                             "refs/guide.md"
                             :description "Reference guide."
                             :content "Guide content."))))))
              (e-harness-activate-capability e-chat-harness capability)
              (cl-letf (((symbol-function 'e-chat-composer--project-file-candidates)
                         (lambda ()
                           (list (list :label "src/example.el"
                                       :path "/tmp/example.el")))))
                (let* ((candidates (e-chat-composer--at-candidates))
                       (labels (mapcar (lambda (candidate)
                                         (plist-get candidate :label))
                                       candidates)))
                  (should (member "file: src/example.el" labels))
                  (should (member "resource: e://reference-capability/refs/guide.md - Reference guide."
                                  labels))
                  (should (member "capability: reference-capability - Reference Capability"
                                  labels)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-at-inserts-file-reference ()
  "Word-boundary @ inserts a selected project file as an inline reference."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-at"))
          (with-current-buffer (e-chat-test--composer buffer)
            (insert "see ")
            (let ((unread-command-events (list ?\r)))
              (cl-letf (((symbol-function 'e-chat-composer--at-candidates)
                         (lambda ()
                           (list (list :label "src/example.el"
                                       :kind 'file
                                       :path "/tmp/example.el"))))
                        ((symbol-function 'e-chat-composer--read-file-reference-text)
                         (lambda (path)
                           (should (equal path "/tmp/example.el"))
                           "(message \"hi\")\n")))
                (e-chat-composer-at)))
            (let* ((document (e-chat-composer-document))
                   (references (plist-get document :references))
                   (submission (plist-get (e-chat-composer-submission)
                                          :prompt)))
              (should (= (length references) 1))
              (should (string-match-p (regexp-quote "src/example.el")
                                      (plist-get document :text)))
              (should (string-match-p (regexp-quote "file:///tmp/example.el")
                                      submission))
              (should (string-match-p (regexp-quote "(message \"hi\")")
                                      submission)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-at-uses-inline-completion-popup ()
  "Word-boundary @ selects files through an inline composer popup."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-at-inline"))
          (with-current-buffer (e-chat-test--composer buffer)
            (insert "see ")
            (let ((unread-command-events (list ?\r)))
              (cl-letf (((symbol-function 'e-chat-composer--at-candidates)
                         (lambda ()
                           (list (list :label "src/example.el"
                                       :kind 'file
                                       :path "/tmp/example.el"))))
                        ((symbol-function 'completing-read)
                         (lambda (&rest _args)
                           (error "composer @ must not use completing-read")))
                        ((symbol-function 'e-chat-composer--read-file-reference-text)
                         (lambda (_path) "(message \"hi\")\n")))
                (e-chat-composer-at)))
            (let ((submission (plist-get (e-chat-composer-submission)
                                         :prompt)))
              (should (string-match-p (regexp-quote "file:///tmp/example.el")
                                      submission)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-at-disambiguates-duplicate-workspace-files ()
  "Duplicate relative file labels remain selectable across workspace roots."
  (let ((primary (file-name-as-directory
                  (make-temp-file "e-chat-prefix-primary-" t)))
        (secondary (file-name-as-directory
                    (make-temp-file "e-chat-prefix-secondary-" t)))
        buffer)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "same.txt" primary)
            (insert "primary\n"))
          (with-temp-file (expand-file-name "same.txt" secondary)
            (insert "secondary\n"))
          (setq buffer (e-chat-test--buffer nil "chat-prefix-duplicate-files"))
          (with-current-buffer (e-chat-test--composer buffer)
            (cl-letf (((symbol-function 'e-chat-composer--workspace-roots)
                       (lambda () (list primary secondary))))
              (let* ((candidates (e-chat-composer--project-file-candidates-sync))
                     (secondary-path (expand-file-name "same.txt" secondary))
                     (secondary-candidate
                      (cl-find secondary-path candidates
                               :key (lambda (candidate)
                                      (plist-get candidate :path))
                               :test #'equal))
                     (secondary-label (plist-get secondary-candidate :label)))
                (should secondary-candidate)
                (should-not (equal secondary-label "same.txt"))
                (let ((unread-command-events (list ?\C-n ?\r)))
                  (cl-letf (((symbol-function 'e-chat-composer--project-file-candidates)
                             (lambda () candidates)))
                    (e-chat-composer-at)))
                (let ((submission (plist-get (e-chat-composer-submission)
                                             :prompt)))
                  (should (string-match-p
                           (regexp-quote (concat "file://" secondary-path))
                           submission))
                  (should (string-match-p (regexp-quote "secondary")
                                          submission)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory primary t)
      (delete-directory secondary t))))





(ert-deftest e-chat-test-composer-at-inserts-resource-reference ()
  "Selecting an e:// resource inserts URI, description, and content context."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-at-resource"))
          (with-current-buffer (e-chat-test--composer buffer)
            (let ((capability
                   (e-capability-create
                    :id 'reference-capability
                    :name "Reference Capability"
                    :resources
                    (list (lambda (store capability)
                            (e-store-register
                             store
                             (e-capability-id capability)
                             "refs/guide.md"
                             :description "Reference guide."
                             :content "Guide content."))))))
              (e-harness-activate-capability e-chat-harness capability)
              (insert "see ")
              (let ((unread-command-events (list ?\r)))
                (cl-letf (((symbol-function 'e-chat-composer--project-file-candidates)
                           (lambda () nil)))
                  (e-chat-composer-at)))
              (let ((submission (plist-get (e-chat-composer-submission)
                                           :prompt)))
                (should (string-match-p
                         (regexp-quote "e://reference-capability/refs/guide.md")
                         submission))
                (should (string-match-p (regexp-quote "Reference guide.")
                                        submission))
                (should (string-match-p (regexp-quote "Guide content.")
                                        submission))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-at-inserts-capability-reference ()
  "Selecting a capability inserts guidance and a lean resource list."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-at-capability"))
          (with-current-buffer (e-chat-test--composer buffer)
            (let ((capability
                   (e-capability-create
                    :id 'reference-capability
                    :name "Reference Capability"
                    :instructions "Use references."
                    :resources
                    (list (lambda (store capability)
                            (e-store-register
                             store
                             (e-capability-id capability)
                             "refs/guide.md"
                             :description "Reference guide."
                             :content "Guide content."))))))
              (e-harness-activate-capability e-chat-harness capability)
              (let ((unread-command-events (list ?\C-n ?\r)))
                (cl-letf (((symbol-function 'e-chat-composer--project-file-candidates)
                           (lambda () nil)))
                  (e-chat-composer-at)))
              (let ((submission (plist-get (e-chat-composer-submission)
                                           :prompt)))
                (should (string-match-p
                         (regexp-quote "The user referenced capability `reference-capability` with @.")
                         submission))
                (should (string-match-p
                         (regexp-quote "consider using the context, actions, tools, or resources provided by this capability")
                         submission))
                (should (string-match-p
                         (regexp-quote "- e://reference-capability/refs/guide.md: Reference guide.")
                         submission))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-at-truncates-file-reference-text ()
  "File reference text is capped with a visible truncation marker."
  (let ((e-chat-file-reference-max-bytes 5)
        (path (make-temp-file "e-chat-file-reference")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "0123456789"))
          (let ((text (e-chat-composer--read-file-reference-text path)))
            (should (string-prefix-p "01234" text))
            (should (string-match-p (regexp-quote "[File reference truncated]")
                                    text))))
      (when (file-exists-p path)
        (delete-file path)))))





(ert-deftest e-chat-test-composer-at-bounds-file-reference-read ()
  "File references read only enough bytes to determine truncation."
  (let (args)
    (cl-letf (((symbol-function 'insert-file-contents-literally)
               (lambda (&rest actual-args)
                 (setq args actual-args)
                 (insert "12345"))))
      (let ((e-chat-file-reference-max-bytes 4))
        (should (string-match-p
                 (regexp-quote "[File reference truncated]")
                 (e-chat-composer--read-file-reference-text "/tmp/example.txt")))))
    (should (equal args '("/tmp/example.txt" nil 0 5)))))





(ert-deftest e-chat-test-composer-slash-expands-leading-selected-prompt ()
  "Leading / expands the selected prompt as editable composer text."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-slash"))
          (with-current-buffer (e-chat-test--composer buffer)
            (let* ((prompt (e-prompt-spec-create
                            :name "review"
                            :description "Review code."
                            :parameters
                            (list (e-prompt-parameter-create
                                   :name "focus"
                                   :description "Focus."))
                            :template "Review ${focus}."))
                   (capability (e-capability-with-prompts-create
                                :id 'review-prompts
                                :name "Review Prompts"
                                :instructions "Use review prompts."
                                :prompts (list prompt))))
              (e-harness-activate-capability e-chat-harness capability)
              (let ((unread-command-events (list ?\r)))
                (cl-letf (((symbol-function 'e-chat-composer--collect-prompt-arguments)
                           (lambda (selected)
                             (should (eq selected prompt))
                             '(("focus" . "regressions")))))
                  (e-chat-composer-slash))))
            (should (equal (e-chat-composer-text)
                           "Review regressions."))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composer-slash-render-error-does-not-insert ()
  "Prompt render errors surface as user errors without partial insertion."
  (let (buffer)
    (unwind-protect
        (progn
          (setq buffer (e-chat-test--buffer nil "chat-prefix-slash-error"))
          (with-current-buffer (e-chat-test--composer buffer)
            (let ((prompt (e-prompt-spec-create
                           :name "needs-topic"
                           :description "Needs topic."
                           :parameters
                           (list (e-prompt-parameter-create
                                  :name "topic"
                                  :description "Topic."))
                           :template "Topic ${topic}.")))
              (let ((unread-command-events (list ?\r)))
                (cl-letf (((symbol-function 'e-chat-composer--prompt-candidates)
                         (lambda () (list (list :label "needs-topic"
                                                :prompt prompt))))
                        ((symbol-function 'e-chat-composer--collect-prompt-arguments)
                         (lambda (_prompt) nil)))
                  (should-error (e-chat-composer-slash) :type 'user-error)
                  (should (string-empty-p (e-chat-composer-text))))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))
(provide 'e-chat-composer-test)

;;; e-chat-composer-test.el ends here
