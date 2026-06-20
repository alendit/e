;;; e-picker.el --- Reusable floating picker primitive -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Presentation-owned picker primitive for e shells.  Callers provide opaque
;; candidates plus callbacks; this module owns rendering, filtering, lifecycle,
;; and the terminal fallback.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup e-picker nil
  "Reusable shell picker primitive."
  :group 'e)

(defface e-picker-title-face
  '((t :inherit minibuffer-prompt :weight bold))
  "Face used for picker titles."
  :group 'e-picker)

(defface e-picker-input-face
  '((t :inherit font-lock-string-face))
  "Face used for picker input."
  :group 'e-picker)

(defface e-picker-selection-face
  '((t :inherit highlight))
  "Face used for the selected picker row."
  :group 'e-picker)

(defface e-picker-meta-face
  '((t :inherit shadow))
  "Face used for secondary picker metadata."
  :group 'e-picker)

(defface e-picker-footer-face
  '((t :inherit shadow))
  "Face used for picker footer text."
  :group 'e-picker)

(defcustom e-picker-default-width 0.6
  "Default picker width as frame fraction or integer columns."
  :type '(choice number integer)
  :group 'e-picker)

(defcustom e-picker-default-height 0.5
  "Default picker height as frame fraction or integer rows."
  :type '(choice number integer)
  :group 'e-picker)

(defcustom e-picker-default-preview-ratio 0.45
  "Default share of the picker body reserved for preview text."
  :type 'number
  :group 'e-picker)

(defvar e-picker--active-buffer nil
  "Currently active picker buffer.")

(defvar-local e-picker--spec nil
  "Picker spec for the current picker buffer.")

(defvar-local e-picker--all-candidates nil
  "All candidates for the current picker buffer.")

(defvar-local e-picker--filtered-candidates nil
  "Filtered candidates for the current picker buffer.")

(defvar-local e-picker--selection 0
  "Current selected candidate index.")

(defvar-local e-picker--input ""
  "Current picker filter input.")

(defvar-local e-picker--window-configuration nil
  "Window configuration captured before opening this picker.")

(defvar-local e-picker--frame nil
  "Child frame displaying the current picker buffer, when known.")

(defvar-local e-picker--candidate-row-starts nil
  "List of buffer positions for rendered candidate rows.")

(defvar-local e-picker--selection-overlay nil
  "Overlay highlighting the selected picker row.")

(defvar-local e-picker--preview-start nil
  "Marker at the start of the rendered preview section.")

(defvar-local e-picker--preview-end nil
  "Marker at the end of the rendered preview section.")

(defvar-local e-picker--closed nil
  "Non-nil when this picker was closed.")

(defvar-local e-picker--disabling-modal-editing nil
  "Non-nil while picker mode is disabling modal editing.")

(defun e-picker--disable-modal-editing ()
  "Disable local modal editing state for picker buffers when available."
  (unless e-picker--disabling-modal-editing
    (setq-local e-picker--disabling-modal-editing t)
    (unwind-protect
        (progn
          (when (fboundp 'evil-local-mode)
            (evil-local-mode -1))
          (when (boundp 'evil-local-mode)
            (setq-local evil-local-mode nil))
          (when (boundp 'evil-state)
            (setq-local evil-state nil)))
      (setq-local e-picker--disabling-modal-editing nil))))

(defun e-picker--enforce-modal-editing-policy ()
  "Disable modal editing if it is reactivated in picker buffers."
  (when (and (derived-mode-p 'e-picker-mode)
             (boundp 'evil-local-mode)
             evil-local-mode)
    (e-picker--disable-modal-editing)))

(defun e-picker--configure-modal-editing-policy ()
  "Configure modal editors to keep `e-picker-mode' in Emacs state."
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'e-picker-mode 'emacs)))

(defun e-picker--make-mode-map (&optional map)
  "Return MAP configured as the keymap for `e-picker-mode'."
  (let ((map (or map (make-sparse-keymap))))
    (suppress-keymap map)
    (define-key map (kbd "C-n") #'e-picker-next)
    (define-key map (kbd "<down>") #'e-picker-next)
    (define-key map (kbd "j") #'e-picker-next)
    (define-key map (kbd "n") nil)
    (define-key map (kbd "C-p") #'e-picker-previous)
    (define-key map (kbd "<up>") #'e-picker-previous)
    (define-key map (kbd "k") #'e-picker-previous)
    (define-key map (kbd "p") nil)
    (define-key map (kbd "RET") #'e-picker-select)
    (define-key map (kbd "<return>") #'e-picker-select)
    (define-key map (kbd "C-g") #'e-picker-cancel)
    (define-key map (kbd "<escape>") #'e-picker-cancel)
    (define-key map (kbd "DEL") #'e-picker-backspace)
    (define-key map (kbd "<backspace>") #'e-picker-backspace)
    (define-key map [remap self-insert-command] #'e-picker-self-insert)
    map))

(defvar e-picker-mode-map nil
  "Keymap for `e-picker-mode'.")

(setq e-picker-mode-map (e-picker--make-mode-map e-picker-mode-map))

(define-derived-mode e-picker-mode special-mode "e-picker"
  "Major mode for e floating picker buffers."
  (setq-local truncate-lines t)
  (setq-local cursor-type nil)
  (add-hook 'evil-local-mode-hook
            #'e-picker--enforce-modal-editing-policy nil t)
  (e-picker--disable-modal-editing)
  (add-hook 'kill-buffer-hook #'e-picker--delete-posframe nil t))

(with-eval-after-load 'evil
  (e-picker--configure-modal-editing-policy))

(defun e-picker--buffer-name (name)
  "Return picker buffer name for NAME."
  (format "*e-picker-%s*" name))

(defun e-picker--posframe-available-p ()
  "Return non-nil when posframe can display a child frame."
  (and (require 'posframe nil t)
       (fboundp 'posframe-workable-p)
       (posframe-workable-p)
       (fboundp 'posframe-show)
       (fboundp 'posframe-hide)))

(defun e-picker--dimension (value frame-size)
  "Return concrete dimension for VALUE against FRAME-SIZE."
  (cond
   ((and (numberp value) (> value 0) (< value 1))
    (max 20 (floor (* frame-size value))))
   ((and (integerp value) (> value 0))
    value)
   (t
    (max 20 (floor (* frame-size 0.6))))))

(defun e-picker-filter-substring (input candidates candidate-key)
  "Return CANDIDATES whose CANDIDATE-KEY contains INPUT.
Matching is case-insensitive.  Empty INPUT returns CANDIDATES unchanged."
  (if (string-empty-p input)
      candidates
    (let ((needle (downcase input)))
      (cl-remove-if-not
       (lambda (candidate)
         (string-match-p
          (regexp-quote needle)
          (downcase (or (funcall candidate-key candidate) ""))))
       candidates))))

(defun e-picker-make-line (left right &optional width)
  "Return one row containing LEFT and right-aligned RIGHT.
WIDTH defaults to the current window body width."
  (let* ((width (max 1 (or width (window-body-width nil t))))
         (left (or left ""))
         (right (or right ""))
         (available-left (max 0 (- width (string-width right) 1)))
         (left (if (> (string-width left) available-left)
                   (truncate-string-to-width left available-left nil nil "...")
                 left))
         (spaces (max 1 (- width (string-width left) (string-width right)))))
    (concat left (make-string spaces ?\s) right)))

(defun e-picker--validate-spec (spec)
  "Validate SPEC and signal `user-error' for malformed picker specs."
  (dolist (key '(:name :candidates :candidate-key :candidate-line :on-select))
    (unless (plist-member spec key)
      (user-error "Picker spec missing %S" key)))
  (unless (symbolp (plist-get spec :name))
    (user-error "Picker :name must be a symbol"))
  (dolist (key '(:candidate-key :candidate-line :on-select))
    (unless (functionp (plist-get spec key))
      (user-error "Picker %S must be a function" key)))
  spec)

(defun e-picker--candidate-source (spec)
  "Return candidate list for SPEC."
  (let ((source (plist-get spec :candidates)))
    (if (functionp source)
        (funcall source)
      source)))

(defun e-picker--filter-candidates ()
  "Update current buffer filtered candidates from current input."
  (let* ((filter (or (plist-get e-picker--spec :filter)
                     #'e-picker-filter-substring))
         (candidate-key (plist-get e-picker--spec :candidate-key)))
    (setq e-picker--filtered-candidates
          (funcall filter e-picker--input e-picker--all-candidates
                   candidate-key))
    (when (>= e-picker--selection (length e-picker--filtered-candidates))
      (setq e-picker--selection
            (max 0 (1- (length e-picker--filtered-candidates)))))))

(defun e-picker--selected-candidate ()
  "Return the currently selected picker candidate, or nil."
  (nth e-picker--selection e-picker--filtered-candidates))

(defun e-picker--render-preview (candidate)
  "Return preview text for CANDIDATE according to current spec."
  (when-let ((preview (plist-get e-picker--spec :preview)))
    (with-temp-buffer
      (funcall preview candidate (current-buffer))
      (buffer-string))))

(defun e-picker--insert-preview-section ()
  "Insert the preview section for the current selected candidate."
  (when-let ((preview-text (e-picker--render-preview
                            (e-picker--selected-candidate))))
    (insert "\n")
    (e-picker--insert-line "Preview" 'e-picker-title-face)
    (insert preview-text)
    (unless (bolp)
      (insert "\n"))))

(defun e-picker--ensure-selection-overlay ()
  "Return the overlay used for the selected picker row."
  (or (and (overlayp e-picker--selection-overlay)
           e-picker--selection-overlay)
      (setq-local e-picker--selection-overlay
                  (let ((overlay (make-overlay (point-min) (point-min)
                                               (current-buffer)
                                               nil t)))
                    (overlay-put overlay 'face 'e-picker-selection-face)
                    (overlay-put overlay 'priority 100)
                    (overlay-put overlay 'evaporate t)
                    overlay))))

(defun e-picker--candidate-row-start (index)
  "Return rendered row start for candidate INDEX, or nil."
  (nth index e-picker--candidate-row-starts))

(defun e-picker--set-candidate-prefix (index selected)
  "Set candidate INDEX prefix according to SELECTED."
  (when-let ((start (e-picker--candidate-row-start index)))
    (save-excursion
      (goto-char start)
      (let ((inhibit-read-only t))
        (delete-char 2)
        (insert (if selected "> " "  "))))))

(defun e-picker--move-selection-overlay ()
  "Move selection overlay and point to the selected candidate row."
  (if-let ((start (e-picker--candidate-row-start e-picker--selection)))
      (let ((overlay (e-picker--ensure-selection-overlay)))
        (save-excursion
          (goto-char start)
          (move-overlay overlay start (line-beginning-position 2)
                        (current-buffer)))
        (goto-char start))
    (when (overlayp e-picker--selection-overlay)
      (delete-overlay e-picker--selection-overlay))))

(defun e-picker--update-preview-section ()
  "Refresh the preview section for the current selected candidate."
  (when (and (markerp e-picker--preview-start)
             (markerp e-picker--preview-end))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char e-picker--preview-start)
        (delete-region e-picker--preview-start e-picker--preview-end)
        (e-picker--insert-preview-section)
        (set-marker e-picker--preview-end (point))))))

(defun e-picker--refresh-selection-display (&optional old-selection)
  "Refresh displayed selection after moving away from OLD-SELECTION."
  (when (and old-selection
             (/= old-selection e-picker--selection))
    (e-picker--set-candidate-prefix old-selection nil))
  (e-picker--set-candidate-prefix e-picker--selection t)
  (e-picker--move-selection-overlay)
  (e-picker--update-preview-section))

(defun e-picker--insert-line (text &optional face)
  "Insert TEXT and newline, optionally applying FACE."
  (let ((start (point)))
    (insert text "\n")
    (when face
      (add-text-properties start (1- (point)) `(font-lock-face ,face)))))

(defun e-picker--render ()
  "Render the current picker buffer."
  (let ((inhibit-read-only t)
        (candidate-line (plist-get e-picker--spec :candidate-line))
        (title (or (plist-get e-picker--spec :title)
                   (symbol-name (plist-get e-picker--spec :name))))
        (footer (or (plist-get e-picker--spec :footer)
                    "RET open  C-g cancel")))
    (setq-local e-picker--candidate-row-starts nil)
    (erase-buffer)
    (e-picker--insert-line title 'e-picker-title-face)
    (e-picker--insert-line (format "> %s" e-picker--input)
                           'e-picker-input-face)
    (insert "\n")
    (if e-picker--filtered-candidates
        (cl-loop
         for candidate in e-picker--filtered-candidates
         for index from 0
         do (let* ((line (funcall candidate-line candidate))
                   (start (point)))
              (push start e-picker--candidate-row-starts)
              (insert (if (= index e-picker--selection) "> " "  ")
                      line
                      "\n")))
      (e-picker--insert-line "No matches" 'e-picker-meta-face))
    (setq-local e-picker--candidate-row-starts
                (nreverse e-picker--candidate-row-starts))
    (setq-local e-picker--preview-start (point-marker))
    (e-picker--insert-preview-section)
    (setq-local e-picker--preview-end (point-marker))
    (insert "\n")
    (e-picker--insert-line footer 'e-picker-footer-face)
    (e-picker--move-selection-overlay)))

(defun e-picker--show-posframe (buffer spec)
  "Show BUFFER as a posframe for SPEC."
  (let* ((width (e-picker--dimension
                 (or (plist-get spec :width) e-picker-default-width)
                 (frame-width)))
         (height (e-picker--dimension
                  (or (plist-get spec :height) e-picker-default-height)
                  (frame-height))))
    (posframe-show buffer
                   :poshandler #'posframe-poshandler-frame-center
                   :width width
                   :height height
                   :accept-focus t
                   :border-width 1)))

(defun e-picker--focus-frame (frame)
  "Give input focus to picker FRAME."
  (when (frame-live-p frame)
    (select-frame-set-input-focus frame)))

(defun e-picker--delete-posframe ()
  "Delete the posframe associated with the current picker buffer."
  (when (and e-picker--frame
             (fboundp 'posframe-delete))
    (posframe-delete (current-buffer))
    (setq-local e-picker--frame nil)))

(defun e-picker-delete (&optional name)
  "Delete picker posframe and buffer for NAME.
When NAME is nil, delete the currently active picker."
  (interactive)
  (let ((buffer (or (and name
                         (get-buffer (e-picker--buffer-name name)))
                    e-picker--active-buffer
                    (and (derived-mode-p 'e-picker-mode)
                         (current-buffer)))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (e-picker--delete-posframe))
      (kill-buffer buffer))))

(defun e-picker--install-action-keys (spec)
  "Install SPEC action keys into the current picker buffer map."
  (let ((map (copy-keymap e-picker-mode-map)))
    (dolist (entry (plist-get spec :actions))
      (let ((key (car entry)))
        (define-key map (vector key)
                    (lambda ()
                      (interactive)
                      (e-picker-dispatch-action key)))))
    (use-local-map map)))

(defun e-picker--close (&optional selected action)
  "Close current picker.
When SELECTED is non-nil, run ACTION after closing."
  (unless e-picker--closed
    (setq e-picker--closed t)
    (when (and e-picker--window-configuration
               (window-configuration-p e-picker--window-configuration))
      (set-window-configuration e-picker--window-configuration))
    (when (and e-picker--frame
               (fboundp 'posframe-hide)
               (buffer-live-p (current-buffer)))
      (posframe-hide (current-buffer))
      (setq-local e-picker--frame nil))
    (when (eq e-picker--active-buffer (current-buffer))
      (setq e-picker--active-buffer nil))
    (when (and selected action)
      (funcall action selected))))

(defun e-picker-next ()
  "Move to the next picker candidate."
  (interactive)
  (when e-picker--filtered-candidates
    (let ((old-selection e-picker--selection))
      (setq e-picker--selection
            (min (1- (length e-picker--filtered-candidates))
                 (1+ e-picker--selection)))
      (unless (= old-selection e-picker--selection)
        (e-picker--refresh-selection-display old-selection)))))

(defun e-picker-previous ()
  "Move to the previous picker candidate."
  (interactive)
  (when e-picker--filtered-candidates
    (let ((old-selection e-picker--selection))
      (setq e-picker--selection (max 0 (1- e-picker--selection)))
      (unless (= old-selection e-picker--selection)
        (e-picker--refresh-selection-display old-selection)))))

(defun e-picker-select ()
  "Select the current picker candidate."
  (interactive)
  (if-let ((candidate (e-picker--selected-candidate)))
      (e-picker--close candidate (plist-get e-picker--spec :on-select))
    (user-error "No picker candidate selected")))

(defun e-picker-cancel ()
  "Cancel the current picker."
  (interactive)
  (e-picker--close))

(defun e-picker-self-insert ()
  "Append the typed character to picker input."
  (interactive)
  (setq e-picker--input
        (concat e-picker--input (string last-command-event)))
  (setq e-picker--selection 0)
  (e-picker--filter-candidates)
  (e-picker--render))

(defun e-picker-backspace ()
  "Delete one character from picker input."
  (interactive)
  (unless (string-empty-p e-picker--input)
    (setq e-picker--input
          (substring e-picker--input 0 (1- (length e-picker--input))))
    (setq e-picker--selection 0)
    (e-picker--filter-candidates)
    (e-picker--render)))

(defun e-picker-dispatch-action (key)
  "Dispatch picker action bound to KEY for the current candidate."
  (interactive
   (list (read-key "Picker action: ")))
  (let ((action (alist-get key (plist-get e-picker--spec :actions))))
    (unless action
      (user-error "No picker action for %s" (single-key-description key)))
    (if-let ((candidate (e-picker--selected-candidate)))
        (let ((keep-open (funcall action candidate)))
          (unless keep-open
            (e-picker--close)))
      (user-error "No picker candidate selected"))))

(defun e-picker--fallback (spec candidates)
  "Use completing-read fallback for SPEC over CANDIDATES."
  (let* ((candidate-key (plist-get spec :candidate-key))
         (candidate-line (plist-get spec :candidate-line))
         (labels (mapcar (lambda (candidate)
                           (funcall candidate-key candidate))
                         candidates))
         (candidate-by-label
          (cl-mapcar #'cons labels candidates))
         (selected-label
          (completing-read
           (format "%s: " (or (plist-get spec :title)
                              (plist-get spec :name)))
           (lambda (string predicate action)
             (if (eq action 'metadata)
                 `(metadata
                   (display-sort-function . identity)
                   (cycle-sort-function . identity)
                   (annotation-function
                    . ,(lambda (label)
                         (when-let ((candidate
                                     (cdr (assoc label candidate-by-label))))
                           (concat " " (funcall candidate-line candidate))))))
               (complete-with-action action labels string predicate)))
           nil
           t))
         (index (cl-position selected-label labels :test #'equal)))
    (when index
      (funcall (plist-get spec :on-select) (nth index candidates)))
    nil))

(defun e-picker-open (&rest spec)
  "Open a picker described by SPEC.
Return the picker buffer when using posframe, or nil when the terminal fallback
handles selection synchronously."
  (setq spec (e-picker--validate-spec spec))
  (let ((candidates (or (e-picker--candidate-source spec) nil)))
    (if (not (e-picker--posframe-available-p))
        (e-picker--fallback spec candidates)
      (let ((buffer (get-buffer-create
                     (e-picker--buffer-name (plist-get spec :name)))))
        (setq e-picker--active-buffer buffer)
        (with-current-buffer buffer
          (e-picker-mode)
          (setq-local e-picker--spec spec)
          (setq-local e-picker--all-candidates candidates)
          (setq-local e-picker--filtered-candidates candidates)
          (setq-local e-picker--selection 0)
          (setq-local e-picker--input "")
          (setq-local e-picker--closed nil)
          (setq-local e-picker--window-configuration
                      (current-window-configuration))
          (e-picker--install-action-keys spec)
          (e-picker--filter-candidates)
          (e-picker--render))
        (let ((frame (e-picker--show-posframe buffer spec)))
          (with-current-buffer buffer
            (setq-local e-picker--frame frame))
          (e-picker--focus-frame frame))
        buffer))))

(defun e-picker--refresh-keymaps ()
  "Refresh picker keymaps after live reload."
  (setq e-picker-mode-map (e-picker--make-mode-map e-picker-mode-map))
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (derived-mode-p 'e-picker-mode)
                   e-picker--spec)
          (e-picker--install-action-keys e-picker--spec))))))

(e-picker--refresh-keymaps)

(provide 'e-picker)

;;; e-picker.el ends here
