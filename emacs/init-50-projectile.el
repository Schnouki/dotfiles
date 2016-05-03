;;; 50-projectile --- Projectile config
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :config
  (setq projectile-keymap-prefix (kbd "s-!")
        projectile-mode-line '(:eval
                              (if
                                  (file-remote-p default-directory)
                                  " Projectile"
                                (format " Pj[%s]"
                                        (projectile-project-name)))))
  (projectile-global-mode))

;;; init-50-projectile.el ends here
