;;; e-runtime-sqlite-p3-test.el --- Feature 87 P3 owner scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-cron)
(require 'e-cron-storage-sqlite)
(require 'e-goodnite-storage-sqlite)
(require 'e-raw-results)
(require 'e-raw-results-storage-sqlite)
(require 'e-runtime-sqlite)
(require 'e-task-queue)
(require 'e-task-storage-sqlite)
(require 'e-voice-adjustment)
(require 'e-voice-storage-sqlite)

(cl-defmacro e-runtime-sqlite-p3-test--with-runtime
    ((runtime directory) &rest body)
  "Run BODY with one disposable RUNTIME rooted at DIRECTORY."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((,directory (make-temp-file "e-runtime-sqlite-p3-" t))
          (,runtime (e-runtime-store-open ,directory)))
     (unwind-protect (progn ,@body)
       (ignore-errors (e-runtime-store-close ,runtime))
       (delete-directory ,directory t))))

(defun e-runtime-sqlite-p3-test--hold-one-response
    (runtime observation release-delay)
  "Hold RUNTIME's next complete response, run OBSERVATION, then release it."
  (let* ((process (e-runtime-store--process runtime))
         (ordinary-filter (process-filter process))
         (captured "")
         held)
    (set-process-filter
     process
     (lambda (worker text)
       (setq captured (concat captured text))
       (when (and (not held) (string-match-p "\n" captured))
         (setq held t)
         (run-at-time 0 nil observation)
         (run-at-time
          release-delay nil
          (lambda ()
            (set-process-filter worker ordinary-filter)
            (funcall ordinary-filter worker captured))))))))














(ert-deftest e-runtime-sqlite-p3-s8-voice-atomic-lru-restart-and-clear ()
  "Voice tells update and evict atomically, survive restart, and clear."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let ((storage (e-voice-storage-sqlite-create runtime)))
      (e-voice-storage-record storage "a" "A" "first" "t1" 2)
      (e-voice-storage-record storage "b" "B" "second" "t2" 2)
      (e-voice-storage-record storage "a" "A2" nil "t3" 2)
      (e-voice-storage-record storage "c" "C" "third" "t4" 2)
      (let ((tells (plist-get (e-voice-storage-list storage) :tells)))
        (should (equal (mapcar (lambda (tell) (plist-get tell :key)) tells)
                       '("c" "a")))
        (should (= (plist-get (cadr tells) :count) 2))
        (should (equal (plist-get (cadr tells) :description) "first")))
      (e-runtime-store-close runtime)
      (setq runtime (e-runtime-store-open directory)
            storage (e-voice-storage-sqlite-create runtime))
      (should (= (plist-get (e-voice-storage-list storage) :count) 2))
      (e-voice-storage-clear storage)
      (should-not (plist-get (e-voice-storage-list storage) :tells)))))


(ert-deftest e-runtime-sqlite-p3-s8-goodnite-checkpoint-before-bounded-cleanup ()
  "Goodnite dedupes demand and resumes cleanup after an ACK-only crash."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let ((storage (e-goodnite-storage-sqlite-create runtime)))
      (should-not
       (plist-get (e-goodnite-storage-append
                   storage "same" '(:kind "read" :ts "one"))
                  :duplicate))
      (should
       (plist-get (e-goodnite-storage-append
                   storage "same" '(:kind "read" :ts "two"))
                  :duplicate))
      (dotimes (index 4)
        (e-goodnite-storage-append
         storage (format "event-%d" index) (list :index index)))
      (let ((page (e-goodnite-storage-page storage 0 2)))
        (should (= (length (plist-get page :events)) 2))
        (should (= (plist-get page :next) 2)))
      (e-goodnite-storage-ack storage 2)
      ;; Crash after checkpoint but before physical cleanup.
      (e-runtime-store-close runtime)
      (setq runtime (e-runtime-store-open directory)
            storage (e-goodnite-storage-sqlite-create runtime))
      (let ((page (e-goodnite-storage-page storage 0 2)))
        (should (= (plist-get page :checkpoint) 2))
        (should (= (plist-get (car (plist-get page :events)) :position) 3)))
      (should (plist-get (e-goodnite-storage-cleanup storage 1) :more))
      (should (= (plist-get (e-goodnite-storage-cleanup storage 1) :deleted) 1))
      (e-runtime-store-close runtime)
      (should-error
       (e-goodnite-storage-append storage "failure" '(:kind "read"))
       :type 'e-runtime-store-unavailable))))

(ert-deftest e-runtime-sqlite-p3-s8-goodnite-observations-remain-distinct ()
  "Equal accesses are distinct observations; re-appending one event id dedupes."
  (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
    (let* ((storage (e-goodnite-storage-sqlite-create runtime))
           (e-goodnite-resources-storage storage)
           (e-goodnite-track-access t)
           (e-goodnite-resources--event-sequence 0)
           (context '(:session-id "s" :turn-id "t"))
           (first (e-goodnite-resources--record-access
                   'search "goodnite://entry" "same" context))
           (second (e-goodnite-resources--record-access
                    'search "goodnite://entry" "same" context)))
      (should-not (equal (plist-get first :event-id)
                         (plist-get second :event-id)))
      (let ((events (plist-get (e-goodnite-storage-page storage 0 10)
                               :events)))
        (should (= (length events) 2))
        (should (equal (mapcar (lambda (event)
                                (plist-get event :position))
                              events)
                       '(1 2))))
      (should
       (plist-get
        (e-goodnite-storage-append
         storage (plist-get first :event-id) '(:duplicate exact-id))
        :duplicate)))))

(ert-deftest e-runtime-sqlite-p3-s8-raw-immutable-fixed-expiry-and-size-boundary ()
  "Raw results dedupe exact content, conflict, expire, and enforce 16 MiB."
  (let ((e-runtime-store-request-timeout 45.0))
    (e-runtime-sqlite-p3-test--with-runtime (runtime directory)
      (let* ((storage (e-raw-results-storage-sqlite-create runtime))
             (uri "raw-result://fixed")
             (first (e-raw-results-storage-put
                     storage uri "same" '(:owner test) 10.0 20.0))
             (duplicate (e-raw-results-storage-put
                         storage uri "same" '(:owner changed) 11.0 99.0)))
        (should-not (plist-get first :duplicate))
        (should (plist-get duplicate :duplicate))
        (should (= (plist-get duplicate :expires-at) 20.0))
        (should-error
         (e-raw-results-storage-put storage uri "different" nil 10.0 20.0)
         :type 'e-raw-results-storage-conflict)
        (let ((exact (make-string (* 16 1024 1024) ?x)))
          (should (= (plist-get
                      (e-raw-results-storage-put
                       storage "raw-result://exact" exact nil 10.0 30.0)
                      :bytes)
                     (string-bytes exact))))
        (should-error
         (e-raw-results-storage-put
          storage "raw-result://over"
          (make-string (1+ (* 16 1024 1024)) ?x) nil 10.0 30.0)
         :type 'e-raw-results-storage-too-large)
        (should-not (e-raw-results-storage-read storage uri 21.0))
        (should (member uri
                        (plist-get (e-raw-results-storage-expire storage 21.0)
                                   :deleted)))))))


(ert-deftest e-runtime-sqlite-p3-composition-injects-one-runtime-and-closes-once ()
  "Every owner port borrows the composition's sole physical runtime."
  (let* ((directory (make-temp-file "e-runtime-sqlite-composition-" t))
         (e-cron-storage nil)
         (e-voice-adjustment-storage nil)
         (e-goodnite-resources-storage nil)
         (e-raw-results-storage nil)
         (e-runtime-sqlite--live-composition nil)
         (composition (e-runtime-sqlite-open directory))
         (runtime (e-runtime-sqlite-runtime-store composition)))
    (unwind-protect
        (progn
          (should
           (eq runtime
               (e-session-storage-runtime-store
                (e-runtime-sqlite-session-store composition))))
          ;; The default remains legacy until the full session command grammar
          ;; is migrated; partial opt-in must not expose unsupported facades.
          (should-not
           (e-session-async-enabled-p
            (e-runtime-sqlite-session-store composition)))
          ;; Board consumers receive this transport through their narrow SQL
          ;; service; the composition exposes no alternate storage adapter.
          (should (e-runtime-store-p runtime))
          (should
           (eq runtime
               (e-task-storage-runtime
                (e-runtime-sqlite-task-storage composition))))
          (should (eq runtime (e-cron-storage-runtime e-cron-storage)))
          (should
           (eq runtime
               (e-voice-storage-runtime e-voice-adjustment-storage)))
          (should
           (eq runtime
               (e-goodnite-storage-runtime e-goodnite-resources-storage)))
          (should
           (eq runtime
               (e-raw-results-storage-runtime e-raw-results-storage)))
          ;; Closing the borrowed session adapter cannot close the runtime.
          (e-session-sqlite-store-close
           (e-runtime-sqlite-session-store composition))
          (should (e-runtime-store-live-p runtime))
          (e-runtime-sqlite-close composition)
          (should-not (e-runtime-store-live-p runtime))
          (should (e-runtime-sqlite-close composition)))
      (ignore-errors (e-runtime-sqlite-close composition))
      (delete-directory directory t))))

(ert-deftest e-runtime-sqlite-p3-composition-constructors-issue-no-domain-requests ()
  "All ordinary owner constructors are pure over a held shared transport."
  (let* ((directory (make-temp-file "e-runtime-sqlite-p3-pure-" t))
         (stall-directory (make-temp-file "e-runtime-sqlite-p3-pure-stall-" t))
         (process-environment (copy-sequence process-environment))
         (e-cron-storage nil)
         (e-voice-adjustment-storage nil)
         (e-goodnite-resources-storage nil)
         (e-raw-results-storage nil)
         (e-runtime-sqlite--live-composition nil)
         runtime composition)
    (setenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY" stall-directory)
    (with-temp-file (expand-file-name "open.hold" stall-directory)
      (insert "hold"))
    (unwind-protect
        (progn
          ;; The complete composition must be able to wire every owner while
          ;; the one transport open remains deliberately held.  Any constructor
          ;; domain request would appear as a second pending or queued entry.
          (setq runtime (e-runtime-store-open directory))
          (let ((deadline (+ (float-time) 2.0)))
            (while (and (< (float-time) deadline)
                        (not (file-exists-p
                              (expand-file-name "open.ready" stall-directory))))
              (accept-process-output nil 0.01))
            (should (file-exists-p
                     (expand-file-name "open.ready" stall-directory))))
          (setq composition
                (e-runtime-sqlite-open directory :runtime-store runtime))
          (let ((active (e-runtime-store--active-request runtime))
                pending-kinds)
            (maphash
             (lambda (_id request)
               (push (e-runtime-store-request--kind request) pending-kinds))
             (e-runtime-store--pending runtime))
            (should active)
            (should (eq (e-runtime-store-request--kind active) 'open))
            (should (eq (e-runtime-store-request--state active) 'submitted))
            (should (equal pending-kinds '(open)))
            (should-not (e-runtime-store--client-queue runtime)))
          (should-not
           (e-session-aggregate-session-values
            (e-runtime-sqlite-session-store composition)))
          (should-not
           (e-task-queue-order (e-runtime-sqlite-task-queue composition)))
          (let ((open-request (e-runtime-store--active-request runtime)))
            (should (eq (e-runtime-store-request--kind open-request) 'open))
            (with-temp-file (expand-file-name "open.release" stall-directory)
              (insert "release"))
            (e-runtime-store-await runtime open-request 2.0)))
      (when composition (ignore-errors (e-runtime-sqlite-close composition)))
      (when (and runtime (not (e-runtime-store--closed runtime)))
        (ignore-errors (e-runtime-store-close runtime)))
      (delete-directory directory t)
      (delete-directory stall-directory t))))

(defun e-runtime-sqlite-p3-test--wait-for-open-stall (stall-directory)
  "Wait for the disposable worker's open control to reach its hold."
  (let ((deadline (+ (float-time) 2.0)))
    (while (and (< (float-time) deadline)
                (not (file-exists-p
                      (expand-file-name "open.ready" stall-directory))))
      (accept-process-output nil 0.01))
    (should (file-exists-p
             (expand-file-name "open.ready" stall-directory)))))

(defun e-runtime-sqlite-p3-test--assert-transport-released (runtime)
  "Assert that RUNTIME and its aggregate admission reservation are empty."
  (should (e-runtime-store--closed runtime))
  (should-not (e-runtime-store--process runtime))
  (should-not (e-runtime-store--opened-process runtime))
  (should-not (e-runtime-store--stderr-buffer runtime))
  (should (= (e-runtime-store--request-count runtime) 0))
  (should (= (e-runtime-store--reserved-bytes runtime) 0))
  (should (= (e-runtime-store--notification-count runtime) 0))
  (should (= (e-runtime-store--reservation-used
              (or (e-runtime-store--reservation runtime)
                  e-runtime-store--default-reservation))
             0)))

(ert-deftest e-runtime-sqlite-p3-composition-accepts-pending-but-rejects-closed-provided-runtime ()
  "A pending transport may be lent, but a closed one is rejected pre-construction."
  (let* ((directory (make-temp-file "e-runtime-sqlite-p3-pending-" t))
         (closed-directory (make-temp-file "e-runtime-sqlite-p3-closed-" t))
         (stall-directory (make-temp-file "e-runtime-sqlite-p3-pending-stall-" t))
         (process-environment (copy-sequence process-environment))
         (e-cron-storage nil)
         (e-voice-adjustment-storage nil)
         (e-goodnite-resources-storage nil)
         (e-raw-results-storage nil)
         (e-runtime-sqlite--live-composition nil)
         pending-runtime pending-composition closed-runtime)
    (setenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY" stall-directory)
    (with-temp-file (expand-file-name "open.hold" stall-directory)
      (insert "hold"))
    (unwind-protect
        (progn
          (setq pending-runtime (e-runtime-store-open directory))
          (e-runtime-sqlite-p3-test--wait-for-open-stall stall-directory)
          (setq pending-composition
                (e-runtime-sqlite-open directory
                                       :runtime-store pending-runtime))
          (should (e-runtime-sqlite-p pending-composition))
          (should-not (e-runtime-sqlite--owns-runtime-store
                       pending-composition))
          ;; Lending a still-opening handle does not force domain I/O or
          ;; transfer caller ownership.
          (e-runtime-sqlite-close pending-composition)
          (should-not (e-runtime-store--closed pending-runtime))
          (with-temp-file (expand-file-name "open.release" stall-directory)
            (insert "release"))
          (e-runtime-store-close pending-runtime)
          (should (e-runtime-store--closed pending-runtime))
          ;; A terminally closed handle is rejected before any owner
          ;; constructor can be reached.  The caller remains its close owner.
          (setq closed-runtime (e-runtime-store-open closed-directory))
          (e-runtime-store-close closed-runtime)
          (let ((constructor-calls 0))
            (cl-letf (((symbol-function 'e-session-sqlite-store-create)
                       (lambda (&rest _arguments)
                         (cl-incf constructor-calls)
                         (error "session constructor must not run"))))
              (should-error
               (e-runtime-sqlite-open closed-directory
                                       :runtime-store closed-runtime)
               :type 'e-runtime-store-unavailable))
            (should (= constructor-calls 0)))
          (should (e-runtime-store--closed closed-runtime))
          (should-not e-runtime-sqlite--live-composition)
          (e-runtime-sqlite-p3-test--assert-transport-released closed-runtime))
      (when pending-composition
        (ignore-errors (e-runtime-sqlite-close pending-composition)))
      (when (and (e-runtime-store-p pending-runtime)
                 (not (e-runtime-store--closed pending-runtime)))
        (ignore-errors (e-runtime-store-close pending-runtime)))
      (when (and (e-runtime-store-p closed-runtime)
                 (not (e-runtime-store--closed closed-runtime)))
        (ignore-errors (e-runtime-store-close closed-runtime)))
      (delete-directory directory t)
      (delete-directory closed-directory t)
      (delete-directory stall-directory t))))

(defun e-runtime-sqlite-p3-test--close-with-session-failure (failure)
  "Close a composition while its session owner signals FAILURE."
  (let* ((directory (make-temp-file "e-runtime-sqlite-p3-close-failure-" t))
         (e-cron-storage nil)
         (e-voice-adjustment-storage nil)
         (e-goodnite-resources-storage nil)
         (e-raw-results-storage nil)
         (e-runtime-sqlite--live-composition nil)
         (composition (e-runtime-sqlite-open directory))
         (runtime (e-runtime-sqlite-runtime-store composition))
         (session-close-count 0)
         (runtime-close-count 0)
         (caught nil)
         (real-runtime-close (symbol-function 'e-runtime-store-close)))
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-sqlite-store-close)
                   (lambda (&rest _arguments)
                     (cl-incf session-close-count)
                     (signal failure (list "synthetic session close failure"))))
                  ((symbol-function 'e-runtime-store-close)
                   (lambda (store)
                     (cl-incf runtime-close-count)
                     (funcall real-runtime-close store))))
          (condition-case err
              (e-runtime-sqlite-close composition)
            (error (setq caught err))
            (quit (setq caught err)))
          (should caught)
          (should (= session-close-count 1))
          (should (= runtime-close-count 1))
          (should (e-runtime-sqlite--closed composition))
          (should-not e-runtime-sqlite--live-composition)
          (should-not e-cron-storage)
          (should-not e-voice-adjustment-storage)
          (should-not e-goodnite-resources-storage)
          (should-not e-raw-results-storage)
          (e-runtime-sqlite-p3-test--assert-transport-released runtime)
          ;; A second close is a no-op and cannot repeat any owner cleanup.
          (should (e-runtime-sqlite-close composition))
          (should (= session-close-count 1))
          (should (= runtime-close-count 1)))
      (ignore-errors (e-runtime-sqlite-close composition))
      (delete-directory directory t))))

(ert-deftest e-runtime-sqlite-p3-close-unwinds-error-and-releases-all-owners ()
  "An owner error does not strand composition or transport resources."
  (e-runtime-sqlite-p3-test--close-with-session-failure 'error))

(ert-deftest e-runtime-sqlite-p3-close-unwinds-quit-and-releases-all-owners ()
  "A quit during owner cleanup does not strand composition or transport resources."
  (e-runtime-sqlite-p3-test--close-with-session-failure 'quit))

(ert-deftest e-runtime-sqlite-p3-composition-borrows-provided-runtime-on-close ()
  "A composition supplied with a transport never becomes its close owner."
  (let* ((directory (make-temp-file "e-runtime-sqlite-borrowed-" t))
         (e-cron-storage nil)
         (e-voice-adjustment-storage nil)
         (e-goodnite-resources-storage nil)
         (e-raw-results-storage nil)
         (e-runtime-sqlite--live-composition nil)
         (runtime (e-runtime-store-open directory))
         composition)
    (unwind-protect
        (progn
          (setq composition (e-runtime-sqlite-open
                             directory :runtime-store runtime))
          (should-not (e-runtime-sqlite--owns-runtime-store composition))
          (e-runtime-sqlite-close composition)
          (should (e-runtime-store-live-p runtime))
          (e-runtime-store-close runtime)
          (should-not (e-runtime-store-live-p runtime)))
      (when composition (ignore-errors (e-runtime-sqlite-close composition)))
      (when (and (e-runtime-store-p runtime)
                 (not (e-runtime-store--closed runtime)))
        (ignore-errors (e-runtime-store-close runtime)))
      (delete-directory directory t))))

(ert-deftest e-runtime-sqlite-p3-composition-rejects-provided-directory-mismatch ()
  "A supplied transport from another directory is rejected without closing it."
  (let* ((runtime-directory (make-temp-file "e-runtime-sqlite-provided-" t))
         (composition-directory (make-temp-file "e-runtime-sqlite-mismatch-" t))
         (e-runtime-sqlite--live-composition nil)
         (runtime (e-runtime-store-open runtime-directory)))
    (unwind-protect
        (progn
          (should-error
           (e-runtime-sqlite-open composition-directory :runtime-store runtime)
           :type 'e-runtime-sqlite-live-composition)
          (should (e-runtime-store-live-p runtime))
          (should-not
           (file-exists-p
            (expand-file-name "store.sqlite3" composition-directory))))
      (when (and (e-runtime-store-p runtime)
                 (not (e-runtime-store--closed runtime)))
        (ignore-errors (e-runtime-store-close runtime)))
      (delete-directory runtime-directory t)
      (delete-directory composition-directory t))))

(ert-deftest e-runtime-sqlite-p3-composition-error-does-not-close-provided-runtime ()
  "A failed owner constructor leaves supplied transport ownership with caller."
  (let* ((directory (make-temp-file "e-runtime-sqlite-error-" t))
         (e-runtime-sqlite--live-composition nil)
         (runtime (e-runtime-store-open directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'e-session-sqlite-store-create)
                   (lambda (&rest _arguments)
                     (error "synthetic session owner failure"))))
          (should-error
           (e-runtime-sqlite-open directory :runtime-store runtime)
           :type 'error)
          (should (e-runtime-store-live-p runtime))
          (should-not e-runtime-sqlite--live-composition))
      (when (and (e-runtime-store-p runtime)
                 (not (e-runtime-store--closed runtime)))
        (ignore-errors (e-runtime-store-close runtime)))
      (delete-directory directory t))))

(ert-deftest e-runtime-sqlite-p3-composition-rejects-second-live-owner ()
  "A second directory cannot replace a live composition's injected adapters."
  (let* ((first-directory (make-temp-file "e-runtime-sqlite-first-" t))
         (second-directory (make-temp-file "e-runtime-sqlite-second-" t))
         (e-cron-storage nil)
         (e-voice-adjustment-storage nil)
         (e-goodnite-resources-storage nil)
         (e-raw-results-storage nil)
         (e-runtime-sqlite--live-composition nil)
         first second)
    (unwind-protect
        (progn
          (setq first (e-runtime-sqlite-open first-directory))
          (let ((first-cron e-cron-storage)
                (first-voice e-voice-adjustment-storage)
                (first-goodnite e-goodnite-resources-storage)
                (first-raw e-raw-results-storage))
            (should-error
             (e-runtime-sqlite-open second-directory)
             :type 'e-runtime-sqlite-live-composition)
            (should (eq e-runtime-sqlite--live-composition first))
            (should (eq e-cron-storage first-cron))
            (should (eq e-voice-adjustment-storage first-voice))
            (should (eq e-goodnite-resources-storage first-goodnite))
            (should (eq e-raw-results-storage first-raw))
            (should-not
             (file-exists-p
              (expand-file-name "store.sqlite3" second-directory))))
          (e-runtime-sqlite-close first)
          (setq second (e-runtime-sqlite-open second-directory))
          (should (eq e-runtime-sqlite--live-composition second))
          (should (e-runtime-store-live-p
                   (e-runtime-sqlite-runtime-store second)))
          (e-runtime-sqlite-close second)
          (should-not e-runtime-sqlite--live-composition))
      (when (and first (not (e-runtime-sqlite--closed first)))
        (ignore-errors (e-runtime-sqlite-close first)))
      (when (and second (not (e-runtime-sqlite--closed second)))
        (ignore-errors (e-runtime-sqlite-close second)))
      (delete-directory first-directory t)
      (delete-directory second-directory t))))

(provide 'e-runtime-sqlite-p3-test)

;;; e-runtime-sqlite-p3-test.el ends here
