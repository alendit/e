;;; e-harness-test-support.el --- Private harness port helpers for tests -*- lexical-binding: t; -*-

;;; Code:

(require 'e-harness)
(require 'e-chat-service)
(require 'e-session)

(defconst e-harness-test--attachment-token 'e-harness-test-attachment)

(defvar e-harness-test--authorized-ports (make-hash-table :test #'eq)
  "Test-only attachment tokens, indexed first by harness identity.")

(defvar e-harness-test--production-authorizer nil
  "Production private-port authorizer wrapped by the test authorizer.")

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
        (e-session-get (e-harness-sessions harness) session-id))
    (e-chat-service-create-session
     :harness harness :id id :metadata metadata)))

(defun e-harness-test--session-tokens (harness)
  "Return the synthetic token table for HARNESS."
  (or (gethash harness e-harness-test--authorized-ports)
      (let ((tokens (make-hash-table :test #'equal)))
        (puthash harness tokens e-harness-test--authorized-ports)
        tokens)))

(defun e-harness-test--real-attachment-token (harness session-id)
  "Return HARNESS SESSION-ID's current production attachment token, if any."
  (when (and (boundp 'e-board-runtime--endpoint-attachments)
             (fboundp 'e-board-runtime--session-key)
             (fboundp 'e-board-runtime--current-attachment-p)
             (fboundp 'e-board-runtime-attachment-endpoint-token))
    (when-let ((attachment
                (gethash (e-board-runtime--session-key harness session-id)
                         e-board-runtime--endpoint-attachments)))
      (and (e-board-runtime--current-attachment-p attachment)
           (e-board-runtime-attachment-endpoint-token attachment)))))

(defun e-harness-test--authorize (harness session-id token)
  "Authorize TOKEN for HARNESS SESSION-ID without weakening real attachments."
  (if (e-harness-test--real-attachment-token harness session-id)
      (and e-harness-test--production-authorizer
           (funcall e-harness-test--production-authorizer
                    harness session-id token))
    (equal token
           (gethash session-id
                    (e-harness-test--session-tokens harness)))))

(defun e-harness-test--install-authorizer ()
  "Install the persistent test wrapper around the production authorizer."
  (unless (eq e-harness--attached-port-authorizer
              #'e-harness-test--authorize)
    (setq e-harness-test--production-authorizer
          e-harness--attached-port-authorizer)
    (setq e-harness--attached-port-authorizer
          #'e-harness-test--authorize)))

(defun e-harness-test--synthetic-token (harness session-id &optional preferred)
  "Register and return a synthetic token for HARNESS SESSION-ID.
PREFERRED wins when non-nil; otherwise reuse an existing token or the test
sentinel.  Real board attachments are never entered into the synthetic table."
  (let* ((tokens (e-harness-test--session-tokens harness))
         (token (or preferred (gethash session-id tokens)
                    e-harness-test--attachment-token)))
    (puthash session-id token tokens)
    token))

(defun e-harness-test--port-token (harness session-id &optional metadata)
  "Return an authorized private-port token for HARNESS SESSION-ID and METADATA."
  (e-harness-test--install-authorizer)
  (if-let ((token (e-harness-test--real-attachment-token harness session-id)))
      ;; Preserve the token only for callbacks that outlive a dynamically bound
      ;; board fixture.  While the attachment is live, the production authorizer
      ;; remains authoritative and the synthetic table is ignored.
      (e-harness-test--synthetic-token harness session-id token)
    (e-harness-test--synthetic-token
     harness session-id (plist-get metadata :board-endpoint-token))))

(cl-defun e-harness-test-prompt-async
    (harness session-id prompt &key delay metadata)
  "Exercise the private attached async port in a core harness unit test."
  (e-harness--prompt-attached-async
   harness session-id prompt :delay delay :metadata metadata
   :attachment-token (e-harness-test--port-token harness session-id metadata)))

(cl-defun e-harness-test-prompt-batch
    (harness session-id prompt &key metadata)
  "Exercise the private attached batch port in a core harness unit test."
  (e-harness--prompt-attached-batch
   harness session-id prompt :metadata metadata
   :attachment-token (e-harness-test--port-token harness session-id metadata)))

(cl-defun e-harness-test-queue-prompt
    (harness session-id prompt &key references metadata)
  "Exercise the private attached queue port in a core harness unit test."
  (e-harness--queue-attached-prompt
   harness session-id prompt :references references :metadata metadata
   :attachment-token (e-harness-test--port-token harness session-id metadata)))

(cl-defun e-harness-test-steer-active-turn
    (harness session-id prompt &key metadata)
  "Exercise the private attached steering port in a core harness unit test."
  (e-harness--steer-attached-turn
   harness session-id prompt :metadata metadata
   :attachment-token (e-harness-test--port-token harness session-id metadata)))

(defun e-harness-test-abort (harness session-id)
  "Exercise the private attached abort port in a core harness unit test."
  (e-harness--abort-attached
   harness session-id (e-harness-test--port-token harness session-id)))

(cl-defun e-harness-test-request-follow-up
    (harness session-id prompt &key references metadata)
  "Exercise the settlement follow-up port in a core harness unit test."
  (e-harness-test--install-authorizer)
  (unless (e-harness-test--real-attachment-token harness session-id)
    (when-let ((entry (gethash session-id (e-harness-active-turns harness))))
      (e-harness-test--synthetic-token
       harness session-id (plist-get entry :endpoint-token))))
  (e-harness--request-attached-follow-up
   harness session-id prompt :references references :metadata metadata))

(e-harness-test--install-authorizer)

(provide 'e-harness-test-support)

;;; e-harness-test-support.el ends here
