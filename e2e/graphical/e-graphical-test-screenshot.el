;;; e-graphical-test-screenshot.el --- Graphical test snapshots -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic visual and structural snapshots of the real frame used by
;; graphical E2E tests.  Native frame export is not available on every Emacs
;; window system, notably NS, so this module renders the observable window
;; geometry, visible text, mode lines, selection, and viewport positions to
;; SVG.  The outer shell runner converts completed SVG artifacts to PNG so
;; image conversion cannot perturb Emacs timers or the behavior under test.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'svg)

(defconst e-graphical-test-screenshot-directory-environment
  "E_GRAPHICAL_E2E_SCREENSHOT_DIR"
  "Environment variable selecting the graphical screenshot artifact directory.")

(defvar e-graphical-test-screenshot-sequence 0
  "Sequence number used to order screenshots in one graphical test process.")

(defvar e-graphical-test-screenshot--pending nil
  "Captured snapshot models awaiting SVG rendering after tests finish.")

(defun e-graphical-test-reset-screenshots ()
  "Reset screenshot ordering and pending models for one graphical test run."
  (setq e-graphical-test-screenshot-sequence 0
        e-graphical-test-screenshot--pending nil))

(defun e-graphical-test-screenshot-directory (&optional directory)
  "Return and create the configured screenshot DIRECTORY.
When DIRECTORY is nil, use `E_GRAPHICAL_E2E_SCREENSHOT_DIR'.  Return nil when
neither source specifies a directory."
  (let ((path (or directory
                  (getenv
                   e-graphical-test-screenshot-directory-environment))))
    (when (and path (not (string-empty-p path)))
      (let ((expanded (file-name-as-directory (expand-file-name path))))
        (make-directory expanded t)
        expanded))))

(defun e-graphical-test-screenshot-enabled-p ()
  "Return non-nil when automatic graphical screenshots are configured."
  (let ((directory
         (getenv e-graphical-test-screenshot-directory-environment)))
    (and directory (not (string-empty-p directory)))))

(defun e-graphical-test-screenshot--label (label)
  "Return LABEL normalized for an artifact file name."
  (let ((normalized
         (replace-regexp-in-string
          "[^[:alnum:]_.-]+" "-" (downcase (format "%s" label)))))
    (string-trim normalized "-+" "-+")))

(defun e-graphical-test-screenshot--next-base (label directory)
  "Return the next artifact base path for LABEL in DIRECTORY."
  (setq e-graphical-test-screenshot-sequence
        (1+ e-graphical-test-screenshot-sequence))
  (expand-file-name
   (format "%03d-%s"
           e-graphical-test-screenshot-sequence
           (or (and-let* ((value (e-graphical-test-screenshot--label label)))
                 (unless (string-empty-p value) value))
               "state"))
   directory))

(defun e-graphical-test-screenshot--visible-lines (window)
  "Return visible display-line text for WINDOW."
  (with-current-buffer (window-buffer window)
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (window-start window))
        (let ((limit (or (window-end window t) (point-max)))
              lines)
          (while (< (point) limit)
            (let ((start (point)))
              (vertical-motion 1 window)
              (when (<= (point) start)
                (goto-char (min limit (1+ start))))
              (push
               (replace-regexp-in-string
                "[\n\r]" ""
                (buffer-substring-no-properties
                 start (min (point) limit)))
               lines)))
          (nreverse lines))))))

(defun e-graphical-test-screenshot--mode-line (window)
  "Return WINDOW's rendered mode-line text without properties."
  (with-current-buffer (window-buffer window)
    (replace-regexp-in-string
     "%%" "%"
     (substring-no-properties
      (format-mode-line mode-line-format nil window (current-buffer)))
     t t)))

(defun e-graphical-test-screenshot--face-color (face attribute frame fallback)
  "Return FACE ATTRIBUTE on FRAME, or FALLBACK when unspecified."
  (let ((value (face-attribute face attribute frame t)))
    (if (or (not (stringp value))
            (string-empty-p value)
            (string= value "unspecified"))
        fallback
      value)))

(defun e-graphical-test-screenshot--frame-state (frame)
  "Capture the visual, window, and viewport state needed to render FRAME."
  (redisplay t)
  (list
   :captured-at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
   :frame-name (frame-parameter frame 'name)
   :frame-position (frame-position frame)
   :frame-text-size (list (frame-width frame) (frame-height frame))
   :frame-size (list (frame-pixel-width frame) (frame-pixel-height frame))
   :frame-fullscreen (frame-parameter frame 'fullscreen)
   :char-size (list (max 1 (frame-char-width frame))
                    (max 1 (frame-char-height frame)))
   :colors
   (list
    :default-background
    (e-graphical-test-screenshot--face-color
     'default :background frame "#ffffff")
    :default-foreground
    (e-graphical-test-screenshot--face-color
     'default :foreground frame "#111111")
    :mode-line-background
    (e-graphical-test-screenshot--face-color
     'mode-line :background frame "#d7d7d7")
    :mode-line-foreground
    (e-graphical-test-screenshot--face-color
     'mode-line :foreground frame "#111111")
    :mode-line-inactive-background
    (e-graphical-test-screenshot--face-color
     'mode-line-inactive :background frame "#eeeeee")
    :mode-line-inactive-foreground
    (e-graphical-test-screenshot--face-color
     'mode-line-inactive :foreground frame "#555555"))
   :selected-buffer (buffer-name (window-buffer (frame-selected-window frame)))
   :windows
   (mapcar
    (lambda (window)
      (list
       :buffer (buffer-name (window-buffer window))
       :selected (eq window (frame-selected-window frame))
       :pixel-edges (window-pixel-edges window)
       :body-pixel-edges (window-body-pixel-edges window)
       :window-start (window-start window)
       :window-end (window-end window t)
       :window-point (window-point window)
       :hscroll (window-hscroll window)
       :vscroll (window-vscroll window t)
       :mode-line (e-graphical-test-screenshot--mode-line window)
       :visible-lines (e-graphical-test-screenshot--visible-lines window)))
    (window-list frame 'nomini))))

(defun e-graphical-test-screenshot--state-svg (state)
  "Return an SVG diagnostic rendering of captured frame STATE."
  (pcase-let* ((`(,width ,height) (plist-get state :frame-size))
               (`(,char-width ,char-height) (plist-get state :char-size))
               (colors (plist-get state :colors))
               (default-bg (plist-get colors :default-background))
               (default-fg (plist-get colors :default-foreground))
               (mode-bg (plist-get colors :mode-line-background))
               (mode-fg (plist-get colors :mode-line-foreground))
               (inactive-bg
                (plist-get colors :mode-line-inactive-background))
               (inactive-fg
                (plist-get colors :mode-line-inactive-foreground))
               (svg (svg-create width height)))
    (svg-rectangle svg 0 0 width height :fill-color default-bg)
    (dolist (window (plist-get state :windows))
      (pcase-let* ((`(,left ,top ,right ,bottom)
                    (plist-get window :pixel-edges))
                   (`(,body-left ,body-top ,body-right ,body-bottom)
                    (plist-get window :body-pixel-edges))
                   (selected (plist-get window :selected))
                   (line-width
                    (max 1 (/ (max 1 (- body-right body-left 8)) char-width)))
                   (line-count
                    (max 0 (/ (max 0 (- body-bottom body-top)) char-height))))
        (svg-rectangle
         svg left top (max 1 (- right left)) (max 1 (- bottom top))
         :fill-color default-bg
         :stroke-color (if selected "#2477d4" "#777777")
         :stroke-width (if selected 3 1))
        (cl-loop
         for line in (plist-get window :visible-lines)
         for index from 0 below line-count
         do
         (svg-text
          svg
          (truncate-string-to-width line line-width nil nil "…")
          :x (+ body-left 4)
          :y (+ body-top (* index char-height) (floor (* char-height 0.8)))
          :fill default-fg
          :font-family "monospace"
          :font-size char-height))
        (when (< body-bottom bottom)
          (svg-rectangle
           svg left body-bottom (max 1 (- right left))
           (max 1 (- bottom body-bottom))
           :fill-color (if selected mode-bg inactive-bg))
          (svg-text
           svg
           (truncate-string-to-width
            (plist-get window :mode-line)
            (max 1 (/ (max 1 (- right left 8)) char-width))
            nil nil "…")
           :x (+ left 4)
           :y (- bottom (max 2 (floor (* char-height 0.2))))
           :fill (if selected mode-fg inactive-fg)
           :font-family "monospace"
           :font-size char-height))))
    (with-temp-buffer
      (svg-print svg)
      (buffer-string))))

(defun e-graphical-test-render-pending-screenshots ()
  "Render all captured screenshot models to their promised SVG paths."
  (dolist (artifact (nreverse e-graphical-test-screenshot--pending))
    (with-temp-file (plist-get artifact :svg)
      (insert
       (e-graphical-test-screenshot--state-svg
        (plist-get artifact :state-data)))))
  (setq e-graphical-test-screenshot--pending nil))

(defun e-graphical-test-capture-state (label &optional directory frame)
  "Capture LABEL for FRAME into DIRECTORY and return its artifact plist.
The result contains `:svg' and `:state' paths.  DIRECTORY defaults to
`E_GRAPHICAL_E2E_SCREENSHOT_DIR'.  Signal an error when no output directory was
configured, so explicit debug captures cannot silently vanish."
  (let* ((directory
          (or (e-graphical-test-screenshot-directory directory)
              (error "Set %s or pass a screenshot directory"
                     e-graphical-test-screenshot-directory-environment)))
         (frame (or frame (selected-frame)))
         (base (e-graphical-test-screenshot--next-base label directory))
         (svg-path (concat base ".svg"))
         (state-path (concat base ".state.el"))
         (state (e-graphical-test-screenshot--frame-state frame)))
    (with-temp-file state-path
      (let ((print-length nil)
            (print-level nil))
        (prin1 state (current-buffer)))
      (insert "\n"))
    (push (list :svg svg-path :state-data state)
          e-graphical-test-screenshot--pending)
    (list :svg svg-path :state state-path)))

(defun e-graphical-test-capture-transition (label function &optional directory)
  "Capture LABEL before and after calling FUNCTION.
Return `(:before ARTIFACTS :after ARTIFACTS :value VALUE)'.  If FUNCTION
signals, capture an `error' state and then re-signal the original condition."
  (let ((before
         (e-graphical-test-capture-state
          (format "%s-before" label) directory)))
    (condition-case err
        (let ((value (funcall function)))
          (sit-for 0.01)
          (redisplay t)
          (list :before before
                :after
                (e-graphical-test-capture-state
                 (format "%s-after" label) directory)
                :value value))
      (error
       (e-graphical-test-capture-state
        (format "%s-error" label) directory)
       (signal (car err) (cdr err))))))

(defun e-graphical-test-capture-automatic-transition (label function)
  "Call FUNCTION, capturing LABEL before and after when debugging is enabled."
  (if (e-graphical-test-screenshot-enabled-p)
      (plist-get
       (e-graphical-test-capture-transition label function)
       :value)
    (funcall function)))

(provide 'e-graphical-test-screenshot)

;;; e-graphical-test-screenshot.el ends here
