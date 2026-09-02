;;; e-session-legacy.el --- Offline legacy session decoder -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure decoder for the retired JSONL session representation.  This module is
;; loaded only by the explicit offline migration application.  It has no write
;; operation, runtime fallback, controller, timer, or session aggregate state.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'e-session-codec)

(define-error 'e-session-legacy-error "Invalid legacy session store")

(defun e-session-legacy--journal-directory (root)
  "Return the retired journal directory below legacy ROOT."
  (expand-file-name "sessions" (file-name-as-directory root)))

(defun e-session-legacy-session-ids (root)
  "Return sorted session identifiers found below copied legacy ROOT."
  (let ((directory (e-session-legacy--journal-directory root)))
    (unless (file-directory-p directory)
      (signal 'e-session-legacy-error
              (list "Legacy session journal root is missing" directory)))
    (sort (mapcar #'file-name-base
                  (directory-files directory t "\\.jsonl\\'"))
          #'string<)))

(defun e-session-legacy-read-records (root session-id)
  "Decode SESSION-ID's complete retired JSONL journal below copied ROOT.

The legacy writer always terminated committed records with a newline.  A
nonempty unterminated tail is therefore an interrupted append and migration
rejects it rather than silently importing a prefix."
  (let ((file (expand-file-name
               (concat session-id ".jsonl")
               (e-session-legacy--journal-directory root))))
    (unless (file-readable-p file)
      (signal 'e-session-legacy-error
              (list "Legacy session journal is missing" file)))
    (with-temp-buffer
      (let ((coding-system-for-read 'no-conversion))
        (insert-file-contents-literally file))
      (when (and (> (buffer-size) 0)
                 (/= (char-before (point-max)) ?\n))
        (signal 'e-session-legacy-error
                (list "Legacy session journal has an incomplete tail" file)))
      (goto-char (point-min))
      (let (records)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (string-empty-p line)
              (condition-case err
                  (push (e-session-codec-json-read-line
                         (decode-coding-string line 'utf-8))
                        records)
                (error
                 (signal 'e-session-legacy-error
                         (list "Malformed legacy session record" file
                               (error-message-string err)))))))
          (forward-line 1))
        (nreverse records)))))

(defun e-session-legacy-read-catalog (root)
  "Decode ROOT's optional retired =index.json= value.

The catalog is a rebuildable projection, but a malformed present file is still
reported so an operator receives a complete source-quality assessment."
  (let ((file (expand-file-name "index.json" (file-name-as-directory root))))
    (when (file-exists-p file)
      (unless (file-readable-p file)
        (signal 'e-session-legacy-error
                (list "Legacy session catalog is unreadable" file)))
      (condition-case err
          (with-temp-buffer
            (let ((coding-system-for-read 'utf-8))
              (insert-file-contents file))
            (json-parse-buffer :object-type 'plist :array-type 'list
                               :null-object e-session-codec-json-null
                               :false-object :json-false))
        (error
         (signal 'e-session-legacy-error
                 (list "Malformed legacy session catalog" file
                       (error-message-string err))))))))

(defun e-session-legacy--checkpoint-file-session-id (file)
  "Return the session id encoded by legacy checkpoint FILE."
  (string-remove-suffix ".checkpoint.json" (file-name-nondirectory file)))

(defun e-session-legacy--journal-position-at-offset (root session-id offset)
  "Translate SESSION-ID's legacy byte OFFSET into a record position."
  (unless (and (integerp offset) (>= offset 0))
    (signal 'e-session-legacy-error
            (list "Invalid legacy checkpoint offset" session-id offset)))
  (let ((journal
         (expand-file-name
          (concat session-id ".jsonl")
          (e-session-legacy--journal-directory root))))
    (unless (file-readable-p journal)
      (signal 'e-session-legacy-error
              (list "Checkpoint has no legacy session journal" session-id)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally journal)
      (when (> offset (buffer-size))
        (signal 'e-session-legacy-error
                (list "Legacy checkpoint offset exceeds journal"
                      session-id offset (buffer-size))))
      (goto-char (1+ offset))
      (unless (or (= offset 0) (= (char-before) ?\n))
        (signal 'e-session-legacy-error
                (list "Legacy checkpoint offset is not a record boundary"
                      session-id offset)))
      (count-lines (point-min) (point)))))

(defun e-session-legacy-read-checkpoints (root)
  "Return sorted active legacy resume checkpoints below ROOT.

Each result retains the parsed source value and supplies a SQLite-ready value
whose physical JSONL byte cursor is translated to the equivalent record
position.  All other semantic fields are preserved exactly."
  (let ((directory (e-session-legacy--journal-directory root)) checkpoints)
    (dolist (file (directory-files directory t "\\.checkpoint\\.json\\'"))
      (let ((session-id (e-session-legacy--checkpoint-file-session-id file)))
        (condition-case err
            (let* ((source-value
                    (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8))
                        (insert-file-contents file))
                      (json-parse-buffer
                       :object-type 'plist :array-type 'list
                       :null-object nil :false-object :json-false)))
                   (offset (plist-get source-value :journal-byte-offset))
                   (position
                    (e-session-legacy--journal-position-at-offset
                     root session-id offset))
                   (value (copy-tree source-value)))
              (plist-put value :journal-byte-offset position)
              (push (list :session-id session-id :source-value source-value
                          :value value)
                    checkpoints))
          (e-session-legacy-error (signal (car err) (cdr err)))
          (error
           (signal 'e-session-legacy-error
                   (list "Malformed legacy session checkpoint" file
                         (error-message-string err)))))))
    (sort checkpoints
          (lambda (left right)
            (string< (plist-get left :session-id)
                     (plist-get right :session-id))))))

(defun e-session-legacy-decode (root)
  "Return validated retired session facts from copied legacy ROOT."
  (let ((ids (e-session-legacy-session-ids root)))
    (list :sessions
          (mapcar (lambda (id)
                    (cons id (e-session-legacy-read-records root id)))
                  ids)
          :checkpoints (e-session-legacy-read-checkpoints root)
          :catalog (e-session-legacy-read-catalog root))))

(provide 'e-session-legacy)

;;; e-session-legacy.el ends here
