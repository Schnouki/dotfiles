;;; 20-text --- text writing
;;; Commentary:
;;; Code:

;; https://manuel-uberti.github.io/emacs/2017/02/04/guess-language/
(use-package flyspell
  :commands (flyspell-mode flyspell-mode-on flyspell-mode-off)
  :init
  (add-hook 'text-mode-hook #'flyspell-mode-on))

(use-package guess-language
  :ensure t
  :defer t
  ;;:diminish guess-language-mode
  :init
  (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-languages '(fr en)
	guess-language-min-paragraph-length 40))

(use-package flycheck-grammalecte
  :ensure t
  :config
  (setq flycheck-grammalecte-download-without-asking t)
  (add-to-list 'flycheck-disabled-checkers 'flycheck-grammalecte))

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
(defun schnouki/toggle-visual-line-mode ()
  "Toggle Visual Line and Visual Fill Column modes in the current buffer."
  (interactive)
  (cond (visual-line-mode
	 (message "Disabling Visual Line & Visual Fill Column modes.")
	 (visual-fill-column-mode -1)
	 (visual-line-mode -1))
	(t
	 (message "Enabling Visual Line & Visual Fill Column modes.")
	 (visual-line-mode 1)
	 (visual-fill-column-mode 1))))

(defun schnouki/set-visual-fill-column-width (width)
  "Set the Visual Fill Column to `WIDTH' in the current buffer.

If called without a prefix argument, the current column is used
as the new width."
  (interactive "NFill column: ")
  (message (format "New column width: %d" (or width (current-column))))
  (setq visual-fill-column-width (or width (current-column)))
  (visual-fill-column--adjust-window))

(bind-keys :map schnouki-prefix-map
	   ("v" . schnouki/toggle-visual-line-mode)
	   ("V" . schnouki/set-visual-fill-column-width))

;;; init-20-text.el ends here
