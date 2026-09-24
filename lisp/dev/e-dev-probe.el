;;; e-dev-probe.el --- Bounded live diagnostics for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fixed, read-only diagnostic projections for external development tools.
;; This deliberately does not accept arbitrary Lisp: every public probe has a
;; reviewed, bounded implementation and returns only a bounded preview value.

;;; Code:

(require 'cl-lib)

(declare-function e-chat-surface-transcript-buffer "e-chat-surface")
(declare-function e-chat-surface-status "e-chat-surface")
(declare-function e-chat-activity-progress-turn-id "e-chat-activity")

(define-error 'e-dev-live-probe-unsupported
  "Unsupported e live probe")

(defconst e-dev-live-probe--max-depth 4
  "Maximum container depth retained in a live-probe result.")

(defconst e-dev-live-probe--max-items 24
  "Maximum items retained from one live-probe container.")

(defconst e-dev-live-probe--max-nodes 128
  "Maximum total values inspected while bounding one live-probe result.")

(defconst e-dev-live-probe--max-string-characters 512
  "Maximum characters retained from one live-probe string.")

(defconst e-dev-live-probe--window-limit 16
  "Maximum windows returned by the `windows' live probe.")

(defun e-dev-live-probe--bounded-string (value)
  "Return a property-free bounded copy of string VALUE."
  (let* ((limit e-dev-live-probe--max-string-characters)
         (truncated (> (length value) limit))
         (text (substring-no-properties
                value 0 (min (length value) limit))))
    (if truncated
        (concat text "\n[e live probe string truncated]")
      text)))

(defun e-dev-live-probe--bounded-value (value depth seen budget)
  "Return a bounded printable projection of VALUE.
DEPTH limits nesting, SEEN prevents cycles, and BUDGET bounds total work."
  (if (<= (aref budget 0) 0)
      '...
    (aset budget 0 (1- (aref budget 0)))
    (cond
     ((stringp value)
      (e-dev-live-probe--bounded-string value))
     ((or (null value) (symbolp value) (numberp value) (characterp value))
      value)
     ((<= depth 0)
      '...)
     ((consp value)
      (if (gethash value seen)
          "#<cycle>"
        (let ((tail value)
              (items nil)
              (count 0)
              cycle)
          (while (and (consp tail)
                      (< count e-dev-live-probe--max-items)
                      (> (aref budget 0) 0)
                      (not cycle))
            (if (gethash tail seen)
                (setq cycle t)
              (puthash tail t seen)
              (push (e-dev-live-probe--bounded-value
                     (car tail) (1- depth) seen budget)
                    items)
              (setq tail (cdr tail))
              (setq count (1+ count))))
          (setq items (nreverse items))
          (cond
           (cycle
            (append items '("#<cycle>")))
           ((consp tail)
            (append items '(...)))
           ((null tail)
            items)
           (t
            (nconc items
                   (e-dev-live-probe--bounded-value
                    tail (1- depth) seen budget)))))))
     ((vectorp value)
      (if (gethash value seen)
          "#<cycle>"
        (puthash value t seen)
        (let* ((count (min (length value) e-dev-live-probe--max-items))
               (items nil))
          (dotimes (index count)
            (push (e-dev-live-probe--bounded-value
                   (aref value index) (1- depth) seen budget)
                  items))
          (apply #'vector
                 (nreverse
                  (if (< count (length value))
                      (cons '... items)
                    items))))))
     ((hash-table-p value)
      (if (gethash value seen)
          "#<cycle>"
        (puthash value t seen)
        (let ((pairs nil)
              (count 0)
              truncated)
          (catch 'done
            (maphash
             (lambda (key entry)
               (if (or (>= count e-dev-live-probe--max-items)
                       (<= (aref budget 0) 0))
                   (progn
                     (setq truncated t)
                     (throw 'done nil))
                 (push
                  (cons
                   (e-dev-live-probe--bounded-value
                    key (1- depth) seen budget)
                   (e-dev-live-probe--bounded-value
                    entry (1- depth) seen budget))
                  pairs)
                 (setq count (1+ count))))
             value))
          (list :hash-table-preview (nreverse pairs)
                :truncated truncated))))
     (t
      (format "#<%s>" (type-of value))))))

(defun e-dev-live-probe--bound (value)
  "Return a printable bounded projection of VALUE."
  (e-dev-live-probe--bounded-value
   value
   e-dev-live-probe--max-depth
   (make-hash-table :test 'eq)
   (vector e-dev-live-probe--max-nodes)))

(defun e-dev-live-probe--buffer-local-value (symbol buffer)
  "Return BUFFER-local SYMBOL when it exists, otherwise nil."
  (when (and (buffer-live-p buffer)
             (local-variable-p symbol buffer))
    (buffer-local-value symbol buffer)))

(defun e-dev-live-probe--selected-buffer ()
  "Return the selected window's live buffer, or nil."
  (when-let* ((window (selected-window)))
    (when (window-live-p window)
      (window-buffer window))))

(defun e-dev-live-probe--selected ()
  "Return bounded scalar state for the selected window and buffer."
  (let* ((window (selected-window))
         (buffer (and (window-live-p window) (window-buffer window))))
    (list :buffer (and (buffer-live-p buffer) (buffer-name buffer))
          :major-mode
          (and (buffer-live-p buffer)
               (buffer-local-value 'major-mode buffer))
          :point
          (and (buffer-live-p buffer)
               (with-current-buffer buffer (point)))
          :buffer-size
          (and (buffer-live-p buffer)
               (with-current-buffer buffer (buffer-size)))
          :window-edges (and (window-live-p window) (window-edges window))
          :frame (and (window-live-p window)
                      (frame-parameter (window-frame window) 'name)))))

(defun e-dev-live-probe--chat ()
  "Return bounded presentation-owned state for the selected chat surface."
  (let* ((selected (e-dev-live-probe--selected-buffer))
         (candidate (and (buffer-live-p selected)
                         (e-chat-surface-transcript-buffer selected)))
         (transcript (if (buffer-live-p candidate) candidate selected)))
    (list :selected-buffer
          (and (buffer-live-p selected) (buffer-name selected))
          :transcript-buffer
          (and (buffer-live-p transcript) (buffer-name transcript))
          :major-mode
          (and (buffer-live-p transcript)
               (buffer-local-value 'major-mode transcript))
          :session-id
          (e-dev-live-probe--buffer-local-value
          'e-chat-session-id transcript)
          :status
          (and (buffer-live-p transcript)
               (e-chat-surface-status transcript))
          :progress-turn-id
          (and (buffer-live-p transcript)
               (e-chat-activity-progress-turn-id transcript))
          :has-harness
          (not (null
                (e-dev-live-probe--buffer-local-value
                 'e-chat-harness transcript))))))

(defun e-dev-live-probe--window-summary (window)
  "Return bounded scalar state for WINDOW."
  (let ((buffer (window-buffer window)))
    (list :buffer (and (buffer-live-p buffer) (buffer-name buffer))
          :selected (eq window (selected-window))
          :edges (window-edges window)
          :dedicated (not (null (window-dedicated-p window)))
          :atomic (not (null (window-parameter window 'window-atom))))))

(defun e-dev-live-probe--windows ()
  "Return a bounded snapshot of windows on the selected frame."
  (let* ((windows (window-list (selected-frame) 'no-minibuffer))
         (count (length windows))
         (visible (cl-subseq windows 0 (min count e-dev-live-probe--window-limit))))
    (list :count count
          :truncated (> count e-dev-live-probe--window-limit)
          :windows (mapcar #'e-dev-live-probe--window-summary visible))))

(defun e-dev-live-probe--symbol (symbol)
  "Return bounded definition state for SYMBOL."
  (unless (symbolp symbol)
    (signal 'wrong-type-argument (list 'symbolp symbol)))
  (list :symbol symbol
        :function-bound (fboundp symbol)
        :variable-bound (boundp symbol)
        :feature-loaded (featurep symbol)
        :function-file (symbol-file symbol 'defun)
        :variable-file (symbol-file symbol 'defvar)))

(defun e-dev-live-probe--dispatch (operation argument)
  "Execute named live probe OPERATION with optional ARGUMENT."
  (pcase operation
    ('ping '(:responsive t))
    ('selected (e-dev-live-probe--selected))
    ('chat (e-dev-live-probe--chat))
    ('windows (e-dev-live-probe--windows))
    ('symbol (e-dev-live-probe--symbol argument))
    (_ (signal 'e-dev-live-probe-unsupported (list operation)))))

(defun e-dev-live-probe--error-result (operation err)
  "Return bounded failure state for OPERATION and condition ERR."
  (let* ((condition (and (consp err) (car err)))
         (message (and (symbolp condition) (get condition 'error-message))))
    (list :ok nil
          :operation (if (symbolp operation) operation 'invalid)
          :condition (if (symbolp condition) condition 'error)
          :message (and (stringp message)
                        (e-dev-live-probe--bounded-string message))
          :details (e-dev-live-probe--bound (cdr-safe err)))))

;;;###autoload
(defun e-dev-live-probe (operation &optional argument)
  "Run fixed read-only live diagnostic OPERATION with optional ARGUMENT.
Return only a bounded printable value.  Errors are converted to bounded
diagnostics without passing their raw condition data to the Emacs server."
  (let ((print-circle t)
        (print-length e-dev-live-probe--max-items)
        (print-level e-dev-live-probe--max-depth)
        (print-escape-newlines t))
    (condition-case nil
        (condition-case err
            (list :ok t
                  :operation (if (symbolp operation) operation 'invalid)
                  :result
                  (e-dev-live-probe--bound
                   (e-dev-live-probe--dispatch operation argument)))
          ((error quit)
           (e-dev-live-probe--error-result operation err)))
      ((error quit)
       '(:ok nil
         :operation internal
         :condition e-dev-live-probe-boundary-failed)))))

(provide 'e-dev-probe)

;;; e-dev-probe.el ends here
