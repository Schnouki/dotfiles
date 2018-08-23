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
  :ensure t)

;;; init-20-text.el ends here
