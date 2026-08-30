;;; e-session-identity.el --- Session identity values -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure session and entry identity generation.  Monotonic ULID state is
;; process-local identity state, not aggregate/session mutation state.

;;; Code:

(defconst e-session-identity--ulid-alphabet "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
  "Crockford Base32 alphabet used for ULID strings.")

(defvar e-session-identity--last-ulid-milliseconds nil
  "Last millisecond timestamp used by `e-session-identity-generate-ulid'.")

(defvar e-session-identity--last-ulid-random nil
  "Last 80-bit random suffix used by `e-session-identity-generate-ulid'.")

(defun e-session-identity--id-timestamp (&optional time)
  "Return TIME as a session-id timestamp."
  (format-time-string "%Y%m%dT%H%M%S" time t))

(defun e-session-identity--generate-id ()
  "Generate a persistent session id."
  (let* ((seed (format "%S" (list (current-time) (random) (emacs-pid)
                                  (system-name))))
         (suffix (substring (secure-hash 'sha1 seed) 0 12)))
    (format "%s-%s" (e-session-identity--id-timestamp) suffix)))

(defun e-session-identity-generate-id ()
  "Return a fresh persistent session id without publishing it.
Application services use this for admission preflight before session creation."
  (e-session-identity--generate-id))

(defun e-session-identity--ulid-encode (number length)
  "Encode NUMBER as a Crockford Base32 string with LENGTH characters."
  (let ((chars (make-string length ?0))
        (index (1- length)))
    (while (>= index 0)
      (aset chars index (aref e-session-identity--ulid-alphabet (logand number 31)))
      (setq number (ash number -5))
      (setq index (1- index)))
    chars))

(defun e-session-identity--current-milliseconds ()
  "Return current Unix time in milliseconds."
  (floor (* 1000 (float-time))))

(defun e-session-identity--random-80-bit ()
  "Return a sufficiently random 80-bit integer."
  (let* ((seed (format "%S" (list (current-time) (random t) (emacs-pid)
                                  (system-name))))
         (hex (substring (secure-hash 'sha1 seed) 0 20)))
    (string-to-number hex 16)))

(defun e-session-identity--ulid-from-parts (milliseconds random)
  "Return a ULID from MILLISECONDS and 80-bit RANDOM suffix."
  (concat (e-session-identity--ulid-encode milliseconds 10)
          (e-session-identity--ulid-encode random 16)))

(defun e-session-identity-generate-ulid ()
  "Generate an opaque monotonic ULID string for durable session entries."
  (let* ((milliseconds (e-session-identity--current-milliseconds))
         (random (if (equal milliseconds e-session-identity--last-ulid-milliseconds)
                     (1+ (or e-session-identity--last-ulid-random 0))
                   (e-session-identity--random-80-bit)))
         (random-limit (expt 2 80)))
    (when (>= random random-limit)
      (setq milliseconds (1+ milliseconds))
      (setq random 0))
    (setq e-session-identity--last-ulid-milliseconds milliseconds
          e-session-identity--last-ulid-random random)
    (e-session-identity--ulid-from-parts milliseconds random)))

(defun e-session-identity--timestamp-milliseconds (timestamp)
  "Return TIMESTAMP parsed as Unix milliseconds, or current milliseconds."
  (condition-case nil
      (if (stringp timestamp)
          (floor (* 1000 (float-time (date-to-time timestamp))))
        (e-session-identity--current-milliseconds))
    (error (e-session-identity--current-milliseconds))))

(defun e-session-identity-legacy-entry-id (session type ordinal timestamp)
  "Return a stable backfilled id for legacy SESSION entry TYPE at ORDINAL."
  (let* ((session-id (plist-get session :id))
         (seed (format "%s:%s:%s:%s" session-id type ordinal timestamp))
         (random (string-to-number (substring (secure-hash 'sha1 seed) 0 20)
                                   16)))
    (e-session-identity--ulid-from-parts
     (e-session-identity--timestamp-milliseconds timestamp)
     random)))


(provide 'e-session-identity)

;;; e-session-identity.el ends here
