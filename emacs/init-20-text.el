;;; 20-text --- text writing
;;; Commentary:
;;; Code:

(use-package flyspell
  :commands (flyspell-mode flyspell-mode-on flyspell-mode-off))

;; https://manuel-uberti.github.io/emacs/2017/02/04/guess-language/
(use-package guess-language
  :ensure t
  :defer t
  :commands (guess-language guess-language-buffer)
  ;;:delight
  :hook text-mode
  :custom
  (guess-language-languages '(en fr))
  (guess-language-min-paragraph-length 40))

;; From https://stackoverflow.com/a/2478549/113325
(defun unfill-paragraph ()
  "Unfill paragraph at or after point."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(bind-key "M-Q" 'unfill-paragraph)

(defun unfill-region ()
  "Unfill each of the paragraphs in the region."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;; Visual line + visual fill column modes
(use-package visual-fill-column
  :ensure t)


(defun schnouki/set-visual-fill-column-width (width)
  "Set the Visual Fill Column to `WIDTH' in the current buffer.

If called without a prefix argument, the current column is used
as the new width."
  (interactive "NFill column: ")
  (message (format "New column width: %d" (or width (current-column))))
  (setq visual-fill-column-width (or width (current-column)))
  (visual-fill-column--adjust-window))

(defun schnouki/toggle-visual-fill-column-center-text ()
  "Toggle the value of `visual-fill-column-center-text'."
  (interactive)
  (setq visual-fill-column-center-text (not visual-fill-column-center-text))
  (when visual-fill-column-mode
    (visual-fill-column--set-margins)))


(define-minor-mode schnouki/visual-text-mode
  "Display text using both Visual Line and Visual Fill Column modes."
  :init-value nil
  :lighter " VT"
  :keymap nil
  (if schnouki/visual-text-mode
      (schnouki/visual-text-mode--enable)
    (schnouki/visual-text-mode--disable)))

(defun schnouki/visual-text-mode--maybe-disable ()
  "Disable `schnouki/visual-text-mode' if its requirements are no longer met."
  (unless (and visual-line-mode visual-fill-column-mode)
    (schnouki/visual-text-mode--disable)))

(defun schnouki/visual-text-mode--enable ()
  "Set up `schnouki/visual-text-mode' for the current buffer."
  (unless visual-line-mode
    (visual-line-mode 1))
  (unless visual-fill-column-mode
    (visual-fill-column-mode 1))
  (add-hook 'visual-line-mode-off-hook #'schnouki/visual-text-mode--maybe-disable 0 t)
  (add-hook 'visual-fill-column-mode-off-hook #'schnouki/visual-text-mode--maybe-disable 0 t))

(defun schnouki/visual-text-mode--disable ()
  "Disable `schnouki/visual-text-mode' for the current buffer."
  (remove-hook 'visual-line-mode-off-hook #'schnouki/visual-text-mode--maybe-disable t)
  (remove-hook 'visual-fill-column-off-mode-hook #'schnouki/visual-text-mode--maybe-disable t)
  (when visual-fill-column-mode
    (visual-fill-column-mode -1))
  (when visual-line-mode
    (visual-line-mode -1)))

(defun schnouki/toggle-visual-text-mode ()
  "Toggle `schnouki/visual-text-mode' in the current buffer."
  (interactive)
  (if schnouki/visual-text-mode
      (schnouki/visual-text-mode -1)
    (schnouki/visual-text-mode 1)))

(defun schnouki/set-wider-fill-column ()
  "Set a wider `fill-column' in some text modes."
  (setq-local fill-column 120))
(add-hook 'markdown-mode-hook #'schnouki/set-wider-fill-column)
(add-hook 'org-mode-hook #'schnouki/set-wider-fill-column)

(bind-keys :map schnouki-prefix-map
           ("v t" . schnouki/toggle-visual-text-mode)
           ("v c" . schnouki/toggle-visual-fill-column-center-text)
           ("v w" . schnouki/set-visual-fill-column-width))

;;; init-20-text.el ends here
