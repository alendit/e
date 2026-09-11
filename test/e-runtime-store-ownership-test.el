;;; e-runtime-store-ownership-test.el --- Atomic runtime-directory claims -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-runtime-store)
(require 'e-runtime-store-offline)
(require 'e-runtime-store-offline-worker)
(require 'e-runtime-store-ownership)
(require 'e-runtime-store-worker)

(defconst e-runtime-store-ownership-test--core-directory
  (expand-file-name "../lisp/core/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory from which disposable claimants load the private claim module.")

(defconst e-runtime-store-ownership-test--file
  (expand-file-name (or load-file-name buffer-file-name))
  "Absolute path of this test source for disposable child processes.")

(defun e-runtime-store-ownership-test--write (file value)
  "Write exact Lisp VALUE to disposable FILE."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (prin1-to-string value) nil file nil 'silent)))

(defun e-runtime-store-ownership-test--read (file)
  "Read one exact Lisp value from disposable FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun e-runtime-store-ownership-test--direct-sibling-attacker ()
  "Try to borrow the owner's claim using only filesystem-visible identity."
  (condition-case nil
      (let* ((directory (getenv "E_BORROW_DIRECTORY"))
             (database-file (expand-file-name "store.sqlite3" directory))
             (metadata
              (e-runtime-store-ownership--read-metadata database-file))
             (lock (e-runtime-store-ownership--parse-lock database-file))
             (actual-parent-pid
              (e-runtime-store-ownership--current-parent-pid))
             (forged-claim
              (e-runtime-store-ownership-claim--create
               :database-file database-file
               :metadata-file
               (e-runtime-store-ownership--metadata-file database-file)
               :runtime-id (plist-get metadata :runtime-id)
               :role 'offline :pid (plist-get metadata :pid)
               :process-start (plist-get metadata :process-start)
               :metadata metadata)))
        (unless (and metadata lock
                     (= (or actual-parent-pid -1)
                        (or (plist-get metadata :pid) -2)))
          (error "Sibling could not inspect the shared ownership identity"))
        ;; This is the narrow library boundary under test.  The sibling can
        ;; reconstruct every disk-visible field, but it cannot receive the
        ;; owner's private control-pipe authorization for the selected worker.
        (let ((store
               (e-runtime-store-open-under-offline-claim
                directory forged-claim :runtime-id "forged-sibling")))
          (e-runtime-store-ownership-test--wait-ready store)
          (ignore-errors (e-runtime-store-close store)))
        (kill-emacs 0))
    (e-runtime-store-owner-identity-conflict (kill-emacs 93))
    (error (kill-emacs 94))))

(defun e-runtime-store-ownership-test--unauthorized-direct-worker ()
  "Run a direct worker sibling and expose whether it reaches `sqlite-open'."
  (let ((real-sqlite-open (symbol-function 'sqlite-open))
        (marker (getenv "E_BORROW_SQLITE_OPEN_MARKER")))
    (condition-case nil
        (cl-letf (((symbol-function 'sqlite-open)
                   (lambda (&rest arguments)
                     (write-region "opened" nil marker nil 'silent)
                     (apply real-sqlite-open arguments))))
          (e-runtime-store-worker-main)
          (kill-emacs 0))
      (e-runtime-store-owner-identity-conflict (kill-emacs 93))
      (error (kill-emacs 94)))))

(defun e-runtime-store-ownership-test--wait-for (predicate)
  "Return non-nil when PREDICATE succeeds within a short deterministic wait."
  (let ((deadline (+ (float-time) 8.0)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(defun e-runtime-store-ownership-test--wait-ready (store)
  "Observe STORE's DP5A cold-open before asserting durable ownership facts."
  (when-let* ((request (e-runtime-store--active-request store))
              ((eq (e-runtime-store-request--kind request) 'open)))
    (e-runtime-store-await store request 5.0)))

(defun e-runtime-store-ownership-test--claimer-script (file)
  "Write the bounded disposable cross-process claimant program at FILE."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region
     (concat
      "(require 'e-runtime-store-ownership)\n"
      "(let* ((database-file (pop command-line-args-left))\n"
      "       (runtime-id (pop command-line-args-left))\n"
      "       (role (intern (pop command-line-args-left)))\n"
      "       (ready-file (pop command-line-args-left))\n"
      "       (go-file (pop command-line-args-left))\n"
      "       (release-file (pop command-line-args-left))\n"
      "       (result-file (pop command-line-args-left)))\n"
      "  (with-temp-file ready-file (insert \"ready\"))\n"
      "  (while (not (file-exists-p go-file)) (sleep-for 0.01))\n"
      "  (condition-case err\n"
      "      (let ((claim (e-runtime-store-ownership-acquire database-file runtime-id role)))\n"
      "        (with-temp-file result-file\n"
      "          (prin1 (list :result 'winner :role role\n"
      "                       :metadata (e-runtime-store-ownership-claim--metadata claim))\n"
      "                 (current-buffer)))\n"
      "        (let ((deadline (+ (float-time) 8.0)))\n"
      "          (while (and (not (file-exists-p release-file))\n"
      "                      (< (float-time) deadline))\n"
      "            (sleep-for 0.01))\n"
      "          (unless (file-exists-p release-file)\n"
      "            (error \"claimant release deadline elapsed\")))\n"
      "        (e-runtime-store-ownership-release claim))\n"
      "    (error\n"
      "     (with-temp-file result-file\n"
      "       (prin1 (list :result 'error :role role\n"
      "                    :symbol (car err) :data (cdr err))\n"
      "              (current-buffer))))))\n")
     nil file nil 'silent)))

(defun e-runtime-store-ownership-test--start-claimer
    (script database-file runtime-id role ready-file go-file release-file result-file)
  "Start one disposable ROLE claimant and return its process and buffer."
  (let ((buffer (generate-new-buffer " *e-runtime-store-ownership-claimant*"))
        (role-entry
         (if (eq role 'offline)
             "(ignore 'e-runtime-store-offline-worker-main)"
           "(ignore 'e-runtime-store-worker-main)")))
    (cons
     (make-process
      :name (format "e-runtime-store-%s-claimant" role)
      :buffer buffer :noquery t
      :command
      (list (expand-file-name invocation-name invocation-directory)
            "-Q" "--batch" "-L" e-runtime-store-ownership-test--core-directory
            "--eval" role-entry "-l" script
            database-file runtime-id (symbol-name role)
            ready-file go-file release-file result-file))
     buffer)))

(ert-deftest e-runtime-store-s92-concurrent-ordinary-offline-claim-has-one-winner ()
  "An ordinary/offline race retains the winner's lock and metadata intact."
  (let* ((directory (make-temp-file "e-runtime-store-ownership-race-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (script (expand-file-name "claimer.el" directory))
         (go-file (expand-file-name "go" directory))
         (release-file (expand-file-name "release" directory))
         (ready-files (list (expand-file-name "ordinary.ready" directory)
                            (expand-file-name "offline.ready" directory)))
         (result-files (list (expand-file-name "ordinary.result" directory)
                             (expand-file-name "offline.result" directory)))
         (processes nil))
    (unwind-protect
        (progn
          (e-runtime-store-ownership-test--claimer-script script)
          (setq processes
                (list
                 (e-runtime-store-ownership-test--start-claimer
                  script database-file "ordinary-race" 'ordinary
                  (nth 0 ready-files) go-file release-file (nth 0 result-files))
                 (e-runtime-store-ownership-test--start-claimer
                  script database-file "offline-race" 'offline
                  (nth 1 ready-files) go-file release-file (nth 1 result-files))))
          (should (e-runtime-store-ownership-test--wait-for
                   (lambda () (cl-every #'file-exists-p ready-files))))
          (write-region "go" nil go-file nil 'silent)
          (should (e-runtime-store-ownership-test--wait-for
                   (lambda () (cl-every #'file-exists-p result-files))))
          (let* ((results (mapcar #'e-runtime-store-ownership-test--read
                                  result-files))
                 (winners (seq-filter
                           (lambda (result) (eq (plist-get result :result) 'winner))
                           results))
                 (losers (seq-filter
                          (lambda (result) (eq (plist-get result :result) 'error))
                          results))
                 (winner (car winners))
                 (loser (car losers)))
            (should (= (length winners) 1))
            (should (= (length losers) 1))
            (should (eq (plist-get loser :symbol)
                        'e-runtime-store-owner-active))
            (should (equal (sort (mapcar (lambda (result)
                                           (plist-get result :role))
                                         results)
                                (lambda (left right)
                                  (string-lessp (symbol-name left)
                                                (symbol-name right))))
                           '(offline ordinary)))
            ;; The loser has already exited, but the live winner still owns
            ;; both artifacts.  A contender cannot delete either one.
            (should (equal
                     (e-runtime-store-ownership--read-metadata database-file)
                     (plist-get winner :metadata)))
            (should (file-symlink-p
                     (e-runtime-store-ownership--lock-file database-file))))
          (write-region "release" nil release-file nil 'silent)
          (should (e-runtime-store-ownership-test--wait-for
                   (lambda () (cl-every (lambda (entry)
                                          (not (process-live-p (car entry))))
                                        processes)))))
      (when (and release-file (not (file-exists-p release-file)))
        (write-region "release" nil release-file nil 'silent))
      (dolist (entry processes)
        (when (process-live-p (car entry))
          (delete-process (car entry)))
        (when (buffer-live-p (cdr entry))
          (kill-buffer (cdr entry))))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-dead-lock-recovers-and-rewrites-metadata ()
  "A dead same-boot claimant is recovered by `lock-file' before SQLite opens."
  (let* ((directory (make-temp-file "e-runtime-store-ownership-stale-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (lock-file (e-runtime-store-ownership--lock-file database-file))
         claim)
    (unwind-protect
        (progn
          ;; Learn this Emacs' boot identity from a real lock, then leave a
          ;; dead-PID lock on that same boot for the primitive to reclaim.
          (lock-file database-file)
          (let* ((identity (e-runtime-store-ownership--parse-lock database-file))
                 (target (format "%s@%s.%d:%d"
                                 (user-login-name) (system-name) 99999999
                                 (plist-get identity :boot))))
            (unlock-file database-file)
            (make-symbolic-link target lock-file))
          (e-runtime-store-ownership-test--write
           (e-runtime-store-ownership--metadata-file database-file)
           '(:runtime-id "dead" :pid 99999999 :process-start (dead)
             :role ordinary))
          (setq claim
                (e-runtime-store-ownership-acquire
                 database-file "recovered" 'ordinary))
          (should (equal
                   (plist-get (e-runtime-store-ownership-claim--metadata claim)
                              :runtime-id)
                   "recovered"))
          (should (eq (file-locked-p database-file) t)))
      (when claim
        (e-runtime-store-ownership-release claim))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-prior-boot-lock-recovers-with-a-live-pid ()
  "A prior-boot lock is reclaimed even when its recorded PID is now live."
  (let* ((directory (make-temp-file "e-runtime-store-ownership-prior-boot-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (lock-file (e-runtime-store-ownership--lock-file database-file))
         claim)
    (unwind-protect
        (progn
          ;; Keep this process' live PID but alter only the boot component of a
          ;; real lock target.  `lock-file' must classify it as stale rather
          ;; than treating a reused PID as a current holder.
          (lock-file database-file)
          (let* ((identity (e-runtime-store-ownership--parse-lock database-file))
                 (target (format "%s@%s.%d:%d"
                                 (user-login-name) (system-name) (emacs-pid)
                                 (1+ (plist-get identity :boot)))))
            (unlock-file database-file)
            (make-symbolic-link target lock-file))
           (e-runtime-store-ownership-test--write
            (e-runtime-store-ownership--metadata-file database-file)
           (list :runtime-id "prior-boot" :pid (emacs-pid)
                 :process-start '(prior boot) :role 'ordinary))
          (setq claim
                (e-runtime-store-ownership-acquire
                 database-file "recovered-prior-boot" 'ordinary))
          (should (equal
                   (plist-get (e-runtime-store-ownership-claim--metadata claim)
                              :runtime-id)
                   "recovered-prior-boot"))
          (should (eq (file-locked-p database-file) t)))
      (when claim
        (e-runtime-store-ownership-release claim))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-live-ordinary-excludes-real-offline-upgrade ()
  "The operator-facing offline worker cannot open SQLite below a live runtime."
  (let* ((directory (make-temp-file "e-runtime-store-live-offline-" t))
         (backup-file (expand-file-name "offline-backup.sqlite3" directory))
         (store (e-runtime-store-open directory))
         (database-file (expand-file-name "store.sqlite3" directory)))
    (unwind-protect
        (progn
          ;; DP5A open returns before the worker claims ownership.  Snapshot
          ;; only after the explicit legacy test observation completes.
          (e-runtime-store-ownership-test--wait-ready store)
          (let ((before (e-runtime-store-ownership--read-metadata database-file)))
          (should-error (e-runtime-store-offline-upgrade directory backup-file)
                        :type 'e-runtime-store-offline-error)
          (should (e-runtime-store-live-p store))
          (should (equal (e-runtime-store-ownership--read-metadata database-file)
                         before))
          (should-not (file-exists-p backup-file))))
      (ignore-errors (e-runtime-store-close store))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-live-pid-mismatch-fails-closed ()
  "A live same-boot PID reuse preserves lock/metadata and reports conflict."
  (let* ((database-file "/tmp/e-runtime-store-pid-reuse.sqlite3")
         (metadata
          '(:runtime-id "old" :pid 4242 :process-start (old 1) :role ordinary))
         (lock (list :host (system-name) :pid 4242 :boot 1))
         writes)
    (cl-letf
        (((symbol-function 'lock-file)
          (lambda (file)
            (funcall (symbol-function 'ask-user-about-lock) file "holder")))
         ((symbol-function 'e-runtime-store-ownership--parse-lock)
          (lambda (_database-file) lock))
         ((symbol-function 'e-runtime-store-ownership--read-metadata)
          (lambda (_database-file) metadata))
         ((symbol-function 'e-runtime-store-ownership--write-metadata)
          (lambda (&rest arguments) (push arguments writes)))
         ((symbol-function 'e-runtime-store-ownership--process-attributes)
          (lambda (pid)
            (if (= pid 4242)
                '((start . (new 2))
                  (args . "emacs --funcall e-runtime-store-worker-main"))
              '((start . (self 3))
                (args . "emacs --funcall e-runtime-store-worker-main"))))))
      (should-error
       (e-runtime-store-ownership-acquire database-file "new" 'ordinary)
       :type 'e-runtime-store-owner-identity-conflict)
      (should-not writes))))

(ert-deftest e-runtime-store-s92-legacy-live-record-remains-protected ()
  "Lockless legacy metadata still protects a demonstrably live worker."
  (let ((database-file "/tmp/e-runtime-store-legacy.sqlite3")
        (metadata '(:runtime-id "legacy" :pid 5151 :started-at 1.0))
        writes unlocks)
    (cl-letf
        (((symbol-function 'e-runtime-store-ownership--acquire-lock)
          (lambda (_database-file) t))
         ((symbol-function 'e-runtime-store-ownership--read-metadata)
          (lambda (_database-file) metadata))
         ((symbol-function 'e-runtime-store-ownership--write-metadata)
          (lambda (&rest arguments) (push arguments writes)))
         ((symbol-function 'unlock-file)
          (lambda (file) (push file unlocks)))
         ((symbol-function 'e-runtime-store-ownership--process-attributes)
          (lambda (_pid)
            '((start . (live 4))
              (args . "emacs --funcall e-runtime-store-worker-main")))))
      (should-error
       (e-runtime-store-ownership-acquire database-file "new" 'ordinary)
       :type 'e-runtime-store-owner-active)
      (should-not writes)
      (should (equal unlocks (list database-file))))))

(ert-deftest e-runtime-store-s92-release-keeps-replaced-metadata-under-own-lock ()
  "An owner never deletes metadata that no longer describes its own claim."
  (let* ((directory (make-temp-file "e-runtime-store-ownership-release-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (metadata-file (e-runtime-store-ownership--metadata-file database-file))
         (replacement '(:runtime-id "new" :pid 2 :process-start (new) :role offline))
         claim)
    (unwind-protect
        (progn
          (setq claim
                (e-runtime-store-ownership-acquire
                 database-file "old" 'ordinary))
          (e-runtime-store-ownership-test--write metadata-file replacement)
          (e-runtime-store-ownership-release claim)
          (setq claim nil)
          (should (equal (e-runtime-store-ownership-test--read metadata-file)
                         replacement))
          (should-not (file-symlink-p
                       (e-runtime-store-ownership--lock-file database-file))))
      (when claim
        (e-runtime-store-ownership-release claim))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-late-release-preserves-replacement-holder ()
  "A stale claimant cannot remove a real replacement holder's lock or metadata."
  (let* ((directory (make-temp-file "e-runtime-store-replacement-holder-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (metadata-file (e-runtime-store-ownership--metadata-file database-file))
         (script (expand-file-name "claimer.el" directory))
         (ready-file (expand-file-name "replacement.ready" directory))
         (go-file (expand-file-name "go" directory))
         (release-file (expand-file-name "release" directory))
         (result-file (expand-file-name "replacement.result" directory))
         (entry nil))
    (unwind-protect
        (progn
          (e-runtime-store-ownership-test--claimer-script script)
          (setq entry
                (e-runtime-store-ownership-test--start-claimer
                 script database-file "replacement" 'offline
                 ready-file go-file release-file result-file))
          (should (e-runtime-store-ownership-test--wait-for
                   (lambda () (file-exists-p ready-file))))
          (write-region "go" nil go-file nil 'silent)
          (should (e-runtime-store-ownership-test--wait-for
                   (lambda () (file-exists-p result-file))))
          (let* ((result (e-runtime-store-ownership-test--read result-file))
                 (replacement-metadata (plist-get result :metadata))
                 (stale-claim
                  (e-runtime-store-ownership-claim--create
                   :database-file database-file :metadata-file metadata-file
                   :metadata
                   '(:runtime-id "old" :pid 1 :process-start (old)
                     :role ordinary))))
            (should (eq (plist-get result :result) 'winner))
            ;; This invokes the real `unlock-file' from a non-owner process.
            ;; The live child must retain both sides of its replacement claim.
            (e-runtime-store-ownership-release stale-claim)
            (should (file-symlink-p
                     (e-runtime-store-ownership--lock-file database-file)))
            (should (equal
                     (e-runtime-store-ownership--read-metadata database-file)
                     replacement-metadata)))
          (write-region "release" nil release-file nil 'silent)
          (should (e-runtime-store-ownership-test--wait-for
                   (lambda () (not (process-live-p (car entry)))))))
      (when (and release-file (not (file-exists-p release-file)))
        (write-region "release" nil release-file nil 'silent))
      (when entry
        (when (process-live-p (car entry))
          (delete-process (car entry)))
        (when (buffer-live-p (cdr entry))
          (kill-buffer (cdr entry))))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-ordinary-closes-before-shared-release ()
  "The ordinary worker claims before SQLite and releases only after close."
  (let ((directory (make-temp-file "e-runtime-store-ownership-order-" t))
        events)
    (unwind-protect
        (let ((e-runtime-store-worker--database nil)
              (e-runtime-store-worker--ownership nil))
          (cl-letf
              (((symbol-function 'sqlite-available-p) (lambda () t))
               ((symbol-function 'e-runtime-store-ownership-acquire)
                (lambda (&rest _arguments) (push 'claim events) 'claim))
               ((symbol-function 'sqlite-open)
                (lambda (&rest _arguments) (push 'sqlite-open events) 'database))
               ((symbol-function 'sqlite-execute) (lambda (&rest _arguments)))
               ((symbol-function 'sqlite-select) (lambda (&rest _arguments)))
               ((symbol-function 'e-runtime-store-worker--schema)
                (lambda (&rest _arguments) (push 'schema events)))
               ((symbol-function 'e-runtime-store-worker--permissions)
                (lambda () (push 'permissions events)))
               ((symbol-function 'sqlite-close)
                (lambda (&rest _arguments) (push 'sqlite-close events)))
               ((symbol-function 'e-runtime-store-ownership-release)
                (lambda (&rest _arguments) (push 'release events))))
            (e-runtime-store-worker--open directory "ordinary-order")
            (e-runtime-store-worker--close)
            (should (equal (nreverse events)
                           '(claim sqlite-open schema permissions
                                   sqlite-close release)))))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-offline-parent-lends-one-exact-claim ()
  "A borrowed ordinary worker neither duplicates nor releases its parent claim."
  (let* ((directory (make-temp-file "e-runtime-store-borrowed-parent-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (initial (e-runtime-store-open directory))
         claim borrowed)
    (unwind-protect
        (progn
          (e-runtime-store-ownership-test--wait-ready initial)
          (e-runtime-store-close initial)
          (setq initial nil
                claim (e-runtime-store-ownership-acquire
                       database-file "offline-repair-test" 'offline)
                borrowed
                (e-runtime-store-open-under-offline-claim
                 directory claim :runtime-id "borrowed-worker-test"))
          (e-runtime-store-ownership-test--wait-ready borrowed)
          (should-error
           (e-runtime-store-ownership-acquire
            database-file "unrelated-owner" 'ordinary)
           :type 'e-runtime-store-owner-active)
          (e-runtime-store-close borrowed)
          (setq borrowed nil)
          (should (eq (file-locked-p database-file) t))
          (should (equal (e-runtime-store-ownership--read-metadata database-file)
                         (e-runtime-store-ownership-claim--metadata claim))))
      (when initial (ignore-errors (e-runtime-store-close initial)))
      (when borrowed (ignore-errors (e-runtime-store-close borrowed)))
      (when claim (e-runtime-store-ownership-release claim))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-borrow-authorization-is-consumed-once ()
  "One private worker authorization cannot permit a second borrowed open."
  (let ((e-runtime-store-worker--borrow-authorization 'authorization))
    (cl-letf (((symbol-function
                'e-runtime-store-ownership--verify-borrow-authorization)
               (lambda (_database authorization)
                 (should (eq authorization 'authorization))
                 t)))
      (should
       (e-runtime-store-worker--consume-borrow-authorization "/tmp/store"))
      (should-error
       (e-runtime-store-worker--consume-borrow-authorization "/tmp/store")
       :type 'e-runtime-store-owner-identity-conflict))))

(ert-deftest e-runtime-store-s92-public-open-rejects-borrow-injection ()
  "The ordinary public open contract has no borrowed-ownership injection."
  (let ((directory (make-temp-file "e-runtime-store-public-borrow-" t)))
    (unwind-protect
        (should-error
         (e-runtime-store-open directory :borrowed-ownership 'forged))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-borrow-authorization-has-no-disk-credential ()
  "Offline borrowing exposes no replayable descriptor or token-file API."
  (dolist (symbol '(e-runtime-store-ownership--write-borrow-token
                    e-runtime-store-ownership--issue-borrowed-child
                    e-runtime-store-ownership--revoke-borrowed-child
                    e-runtime-store-ownership--read-borrow-token
                    e-runtime-store-ownership--verify-borrowed-parent
                    e-runtime-store--borrowed-ownership))
    (should-not (fboundp symbol))))

(ert-deftest e-runtime-store-s92-direct-sibling-cannot-borrow-selected-worker-claim ()
  "A direct sibling sees disk identity but not the selected worker's handshake."
  (let* ((directory (make-temp-file "e-runtime-store-borrowed-sibling-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (initial (e-runtime-store-open directory))
         claim borrowed)
    (unwind-protect
        (progn
          (e-runtime-store-ownership-test--wait-ready initial)
          (e-runtime-store-close initial)
          (setq initial nil
                claim (e-runtime-store-ownership-acquire
                       database-file "offline-repair-test" 'offline)
                borrowed
                (e-runtime-store-open-under-offline-claim
                 directory claim :runtime-id "selected-borrowed-worker"))
          (e-runtime-store-ownership-test--wait-ready borrowed)
          (should-not
           (seq-find
            (lambda (file) (string-match-p "\\.borrow-" file))
            (directory-files directory)))
          (let ((process-environment (copy-sequence process-environment)))
            (setenv "E_BORROW_DIRECTORY" directory)
            ;; `process-file' makes this Emacs a true direct child of the same
            ;; claim-owning parent as the selected runtime worker.
            (should
             (= 93
                (process-file
                 (expand-file-name invocation-name invocation-directory)
                 nil nil nil "-Q" "--batch" "-L"
                 e-runtime-store-ownership-test--core-directory
                 "--eval" "(setq load-prefer-newer t)"
                 "-l" e-runtime-store-ownership-test--file
                 "--funcall"
                 "e-runtime-store-ownership-test--direct-sibling-attacker")))))
      (when initial (ignore-errors (e-runtime-store-close initial)))
      (when borrowed (ignore-errors (e-runtime-store-close borrowed)))
      (when claim (e-runtime-store-ownership-release claim))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-direct-worker-needs-private-pipe-authorization ()
  "A direct worker sibling cannot open from the borrowed marker alone."
  (let* ((directory (make-temp-file "e-runtime-store-borrowed-worker-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (input-file (expand-file-name "unauthorized-open.input" directory))
         (sqlite-open-marker
          (expand-file-name "unauthorized-sqlite-opened" directory))
         (initial (e-runtime-store-open directory))
         claim borrowed)
    (unwind-protect
        (progn
          (e-runtime-store-ownership-test--wait-ready initial)
          (e-runtime-store-close initial)
          (setq initial nil
                claim (e-runtime-store-ownership-acquire
                       database-file "offline-repair-test" 'offline)
                borrowed
                (e-runtime-store-open-under-offline-claim
                 directory claim :runtime-id "selected-borrowed-worker"))
          (e-runtime-store-ownership-test--wait-ready borrowed)
          (let ((open-request
                 (list :id "unauthorized-open" :kind 'open
                       :directory directory :runtime-id "direct-sibling"
                       :access-mode 'read-write
                       :parent-identity (e-runtime-store--parent-identity)
                       :borrowed-authorized t)))
            (write-region (e-runtime-store--pack open-request)
                          nil input-file nil 'silent))
          (let ((process-environment (copy-sequence process-environment))
                (output (generate-new-buffer
                         " *e-runtime-store-unauthorized-worker*")))
            (unwind-protect
                (progn
                  (setenv "E_BORROW_SQLITE_OPEN_MARKER" sqlite-open-marker)
                  ;; This is a second direct child of the claim owner, just
                  ;; like the selected worker, but its private input contains
                  ;; only an open.  Protocol errors are returned normally.
                  (should
                   (= 0
                      (process-file
                       (expand-file-name invocation-name invocation-directory)
                       input-file output nil "-Q" "--batch" "-L"
                       e-runtime-store-ownership-test--core-directory
                       "--eval" "(setq load-prefer-newer t)"
                       "-l" e-runtime-store-ownership-test--file
                       "--funcall"
                       "e-runtime-store-ownership-test--unauthorized-direct-worker")))
                  (with-current-buffer output
                    (goto-char (point-min))
                    (let ((response
                           (e-runtime-store--unpack
                            (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position)))))
                      (should-not (plist-get response :ok))
                      (should
                       (eq (plist-get response :error-symbol)
                           'e-runtime-store-owner-identity-conflict)))))
              (kill-buffer output)))
          (should-not (file-exists-p sqlite-open-marker)))
      (when initial (ignore-errors (e-runtime-store-close initial)))
      (when borrowed (ignore-errors (e-runtime-store-close borrowed)))
      (when claim (e-runtime-store-ownership-release claim))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-current-lock-missing-metadata-fails-closed ()
  "A same-process current-boot lock remains live without adjacent metadata."
  (let* ((directory (make-temp-file "e-runtime-store-missing-owner-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         claim)
    (unwind-protect
        (progn
          (with-temp-file database-file)
          (setq claim
                (e-runtime-store-ownership-acquire
                 database-file "current-owner" 'offline))
          (delete-file (e-runtime-store-ownership--metadata-file database-file))
          (should-error
           (e-runtime-store-ownership-acquire
            database-file "replacement" 'ordinary)))
      (when claim
        (e-runtime-store-ownership--write-metadata
         database-file (e-runtime-store-ownership-claim--metadata claim))
        (e-runtime-store-ownership-release claim))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-current-lock-corrupt-metadata-fails-closed ()
  "A same-process current-boot lock remains live with corrupt metadata."
  (let* ((directory (make-temp-file "e-runtime-store-corrupt-owner-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         claim)
    (unwind-protect
        (progn
          (with-temp-file database-file)
          (setq claim
                (e-runtime-store-ownership-acquire
                 database-file "current-owner" 'offline))
          (with-temp-file
              (e-runtime-store-ownership--metadata-file database-file)
            (insert "not-a-plist"))
          (should-error
           (e-runtime-store-ownership-acquire
            database-file "replacement" 'ordinary)))
      (when claim
        (e-runtime-store-ownership--write-metadata
         database-file (e-runtime-store-ownership-claim--metadata claim))
        (e-runtime-store-ownership-release claim))
      (delete-directory directory t))))

(ert-deftest e-runtime-store-s92-offline-closes-before-shared-release ()
  "The offline upgrade claims before its first SQLite open and releases last."
  (let* ((directory (make-temp-file "e-runtime-store-offline-order-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (backup-file (expand-file-name "backup/store.sqlite3" directory))
         (events nil))
    (unwind-protect
        (progn
          (e-runtime-store-worker--open directory "offline-fixture")
          (e-runtime-store-worker--close)
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (progn
                  (sqlite-execute
                   database
                   "UPDATE store_meta SET value='4' WHERE key='schema_version'")
                  (sqlite-execute
                   database
                   "DELETE FROM schema_migrations WHERE version>=5")
                  (sqlite-execute database "DROP TABLE runtime_store_receipts")
                  (sqlite-execute database "DROP TABLE runtime_store_state"))
              (sqlite-close database)))
          (let ((real-acquire
                 (symbol-function 'e-runtime-store-ownership-acquire))
                (real-release
                 (symbol-function 'e-runtime-store-ownership-release))
                (real-open (symbol-function 'sqlite-open))
                (real-close (symbol-function 'sqlite-close)))
            (cl-letf
                (((symbol-function 'e-runtime-store-ownership-acquire)
                  (lambda (&rest arguments)
                    (push 'claim events)
                    (apply real-acquire arguments)))
                 ((symbol-function 'sqlite-open)
                  (lambda (&rest arguments)
                    (push 'sqlite-open events)
                    (apply real-open arguments)))
                 ((symbol-function 'sqlite-close)
                  (lambda (&rest arguments)
                    (push 'sqlite-close events)
                    (apply real-close arguments)))
                 ((symbol-function 'e-runtime-store-ownership-release)
                  (lambda (&rest arguments)
                    (push 'release events)
                    (apply real-release arguments))))
              (e-runtime-store-offline-worker--upgrade database-file backup-file)))
          (let ((ordered (nreverse events)))
            (should (eq (car ordered) 'claim))
            (should (eq (cadr ordered) 'sqlite-open))
            (should (eq (car (last ordered)) 'release))
            (should (= (cl-count 'sqlite-close ordered) 2))))
      (e-runtime-store-worker--close)
      (delete-directory directory t))))

(provide 'e-runtime-store-ownership-test)

;;; e-runtime-store-ownership-test.el ends here
