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

;;; init-20-text.el ends here
