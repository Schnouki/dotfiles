;;; 50-projectile --- Projectile config
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "s-!")
        projectile-mode-line '(:eval
                              (if
                                  (file-remote-p default-directory)
                                  " Projectile"
                                (format " Pj[%s]"
                                        (projectile-project-name)))))
  :config
  (projectile-global-mode))

;;; init-50-projectile.el ends here
