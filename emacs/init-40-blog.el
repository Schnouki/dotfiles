;;; 40-blog --- blogging configuration
;;; Commentary:
;;; Code:

(use-package easy-hugo
  :ensure t
  :bind ("C-! e" . easy-hugo)
  :init
  (setq easy-hugo-basedir "~/blog/"
	easy-hugo-url "https://schnouki.net"
	easy-hugo-sshdomain "ks"
	easy-hugo-root "/srv/http/schnouki.net/htdocs-hugo"
	easy-hugo-image-directory "img"))

;; Useless :)
(use-package company-emoji
  :ensure t
  :commands company-emoji-init
  :config
  (setq company-emoji-insert-unicode nil)
  :init
  (add-hook 'markdown-mode-hook 'company-emoji-init))

;;; init-40-blog.el ends here
