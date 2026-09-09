;;; e-harness-test-support.el --- Attached-turn test port helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'e-harness)
(require 'e-harness-turn)
(require 'e-chat-service)
(require 'e-session)

(defconst e-harness-test--attachment-token 'e-harness-test-attachment)

(defvar e-harness-test--authorized-ports (make-hash-table :test #'eq)
  "Test-only attachment tokens, indexed first by harness identity.")

(defvar e-harness-test--follow-up-publisher nil
  "Optional test-local continuation publisher used by attached-turn tests.
This is intentionally a test fixture variable; production attachment ports
are explicit values and never consult a process-global callback.")

(cl-defun e-harness-test-create-board-session
    (harness &key id metadata board-id principal)
  "Create a board-native test session on HARNESS and return its session value.
ID and METADATA match `e-harness-create-session'.  BOARD-ID and PRINCIPAL may
pin an already-created board's durable identity.  The ordinary path exercises
the same board/session creation service used by presentation shells."
  (if board-id
      (let* ((session
              (e-harness-create-session harness :id id :metadata metadata))
             (session-id (plist-get session :id)))
        (e-session-declare-board-state
         (e-harness-sessions harness) session-id principal board-id)
        (e-session-local-state (e-harness-sessions harness) session-id))
    (e-chat-service-create-ephemeral-session
     :harness harness :id id :metadata metadata)))

(defun e-harness-test--session-tokens (harness)
  "Return the synthetic token table for HARNESS."
  (or (gethash harness e-harness-test--authorized-ports)
      (let ((tokens (make-hash-table :test 'equal)))
        (puthash harness tokens e-harness-test--authorized-ports)
        tokens)))

(defun e-harness-test--synthetic-token (harness session-id &optional preferred)
  "Register and return a synthetic token for HARNESS SESSION-ID.
PREFERRED wins when non-nil; otherwise reuse an existing token or the test
sentinel."
  (let* ((tokens (e-harness-test--session-tokens harness))
         (token (or preferred (gethash session-id tokens)
                    e-harness-test--attachment-token)))
    (puthash session-id token tokens)
    token))

(defun e-harness-test--production-attachment (harness session-id)
  "Return the current board attachment for HARNESS SESSION-ID, if loaded."
  (when (and (boundp 'e-board-runtime--endpoint-attachments)
             (fboundp 'e-board-runtime--session-key)
             (fboundp 'e-board-runtime--current-attachment-p)
             (fboundp 'e-board-runtime-attachment-turn-port))
    (when-let ((attachment
                (gethash (e-board-runtime--session-key harness session-id)
                         e-board-runtime--endpoint-attachments)))
      (and (e-board-runtime--current-attachment-p attachment)
           attachment))))

(defun e-harness-test--port-token (harness session-id &optional metadata)
  "Return the current production or synthetic token for HARNESS SESSION-ID."
  (if-let ((attachment (e-harness-test--production-attachment
                        harness session-id)))
      (e-board-runtime-attachment-endpoint-token attachment)
    (e-harness-test--synthetic-token
     harness session-id (plist-get metadata :board-endpoint-token))))

(defun e-harness-test--attached-turn-port (harness session-id &optional metadata)
  "Return a fresh explicit test port for HARNESS SESSION-ID.
The port authorizes only the token registered by this test fixture.  Its
continuation publisher is test-local and can be dynamically replaced without
mutating any production callback slot."
  (if-let ((attachment (e-harness-test--production-attachment
                        harness session-id)))
      (e-board-runtime-attachment-turn-port attachment)
    (let ((token (e-harness-test--synthetic-token
                  harness session-id (plist-get metadata :board-endpoint-token))))
      (e-harness-attached-turn-port-create
       :harness harness
       :session-id session-id
       :attachment-token token
       :authorizer
       (lambda (candidate-harness candidate-session-id candidate-token)
         (and (eq candidate-harness harness)
              (equal candidate-session-id session-id)
              (equal candidate-token token)))
       :follow-up-publisher
       (lambda (candidate-harness candidate-session-id prompt &rest args)
         (if e-harness-test--follow-up-publisher
             (apply e-harness-test--follow-up-publisher
                    candidate-harness candidate-session-id prompt args)
           (apply #'e-harness-attached-turn-follow-up
                  candidate-harness candidate-session-id prompt args)))))))

(cl-defun e-harness-test-prompt-async
    (harness session-id prompt &key delay metadata)
  "Exercise the attached async port in a core harness unit test."
  (e-harness-attached-turn-port-submit
   (e-harness-test--attached-turn-port harness session-id metadata)
   prompt :delay delay :metadata metadata))

(cl-defun e-harness-test-prompt-batch
    (harness session-id prompt &key metadata)
  "Exercise the attached batch port in a core harness unit test."
  (e-harness-attached-turn-port-submit-batch
   (e-harness-test--attached-turn-port harness session-id metadata)
   prompt :metadata metadata))

(cl-defun e-harness-test-queue-prompt
    (harness session-id prompt &key references metadata)
  "Exercise the attached queue port in a core harness unit test."
  (e-harness-attached-turn-port-queue
   (e-harness-test--attached-turn-port harness session-id metadata)
   prompt :references references :metadata metadata))

(cl-defun e-harness-test-steer-active-turn
    (harness session-id prompt &key metadata)
  "Exercise the attached steering port in a core harness unit test."
  (e-harness-attached-turn-port-steer
   (e-harness-test--attached-turn-port harness session-id metadata)
   prompt :metadata metadata))

(defun e-harness-test-abort (harness session-id)
  "Exercise the attached abort port in a core harness unit test."
  (e-harness-attached-turn-port-abort
   (e-harness-test--attached-turn-port harness session-id)))

(cl-defun e-harness-test-request-follow-up
    (harness session-id prompt &key references metadata)
  "Exercise the settlement follow-up port in a core harness unit test."
  (let* ((entry (gethash session-id (e-harness-active-turns harness)))
         (token (or (and (listp entry) (plist-get entry :endpoint-token))
                    (e-harness-test--synthetic-token
                     harness session-id
                     (plist-get metadata :board-endpoint-token))))
         (port (or (and (listp entry)
                        (e-harness-attached-turn-port-p
                         (plist-get entry :attached-turn-port))
                        (plist-get entry :attached-turn-port))
                   (e-harness-test--attached-turn-port
                    harness session-id metadata))))
    (when (listp entry)
      (plist-put entry :endpoint-token token)
      (plist-put entry :attached-turn-port port))
    (e-harness-attached-turn-port-follow-up
     port prompt :references references :metadata metadata)))

(provide 'e-harness-test-support)

;;; e-harness-test-support.el ends here
