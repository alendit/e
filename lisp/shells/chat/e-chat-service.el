;;; e-chat-service.el --- Shell-neutral chat services for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Small shell-neutral operations shared by chat presentation shells.
;; Text-buffer presentation, composer editing, rerendering, and point-based
;; navigation stay in `e-chat.el'.  This module only delegates to the
;; chat-session capability and default chat harness registry.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-chat-output-mode)
(require 'e-chat-session)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)

(defvar e-chat-default-harness-id)

(defgroup e-chat-service nil
  "Shell-neutral chat service operations."
  :group 'e)

(defcustom e-chat-service-default-harness-id :chat-default
  "Harness registry id used by shell-neutral default chat commands."
  :type 'symbol
  :group 'e-chat-service)

(defun e-chat-service--harness-has-capability-p (harness capability-id)
  "Return non-nil when HARNESS has active capability CAPABILITY-ID."
  (memq capability-id
        (mapcar #'e-capability-id
                (e-harness-active-capabilities harness))))

(defun e-chat-service--harness-for-instance (instance)
  "Return the live chat harness for INSTANCE."
  (let* ((instance-id (e-harness-instance-id instance))
         (harness
          (condition-case err
              (e-harness-instance-get-or-create instance-id)
            ((e-harness-instance-missing e-harness-registry-missing)
             (user-error "No e harness registered for %S" (cadr err))))))
    (unless (e-chat-service--harness-has-capability-p harness 'chat-session)
      (user-error "Harness instance %S does not provide chat-session capability"
                  instance-id))
    harness))

(defun e-chat-service-default-harness-id ()
  "Return the effective default chat harness id."
  (if (boundp 'e-chat-default-harness-id)
      e-chat-default-harness-id
    e-chat-service-default-harness-id))

(defun e-chat-service-default-harness ()
  "Return the configured default chat harness."
  (let* ((harness-id (e-chat-service-default-harness-id))
         (instance (e-harness-instance-get harness-id))
         (harness
          (if instance
              (e-chat-service--harness-for-instance instance)
            (condition-case err
                (e-harness-registry-get-or-create harness-id)
              (e-harness-registry-missing
               (user-error "No e harness registered for %S" (cadr err)))))))
    (unless (e-chat-service--harness-has-capability-p harness 'chat-session)
      (user-error "Harness %S does not provide chat-session capability"
                  harness-id))
    harness))

(cl-defun e-chat-service-create-session (&key harness metadata id)
  "Create and return a chat session in HARNESS with METADATA and optional ID."
  (e-harness-create-session
   (or harness (e-chat-service-default-harness))
   :id id
   :metadata metadata))

(cl-defun e-chat-service-submit-session
    (harness session-id prompt &key references delay metadata)
  "Submit PROMPT with REFERENCES and METADATA to HARNESS SESSION-ID."
  (e-chat-session-submit
   harness session-id prompt
   :references references
   :delay delay
   :metadata metadata))

(cl-defun e-chat-service-steer-session
    (harness session-id prompt &key metadata)
  "Steer active HARNESS SESSION-ID with PROMPT and METADATA."
  (e-chat-session-steer harness session-id prompt :metadata metadata))

(cl-defun e-chat-service-queue-session
    (harness session-id prompt &key references metadata)
  "Queue PROMPT with REFERENCES and METADATA for HARNESS SESSION-ID."
  (e-chat-session-queue
   harness session-id prompt
   :references references
   :metadata metadata))

(defun e-chat-service-abort-session (harness session-id)
  "Abort active turn for HARNESS SESSION-ID."
  (e-chat-session-abort harness session-id))

(defun e-chat-service-output-mode (harness session-id)
  "Return effective assistant output mode for HARNESS SESSION-ID."
  (e-chat-output-mode-resolve harness session-id))

(defun e-chat-service-set-output-mode (harness session-id mode)
  "Set assistant output MODE for HARNESS SESSION-ID."
  (e-chat-output-mode-session-set harness session-id mode))

(provide 'e-chat-service)

;;; e-chat-service.el ends here
