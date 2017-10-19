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
  (--each '(".pyc" ".o" ".so" "~" "#" ".min.js")
    (add-to-list 'projectile-globally-ignored-file-suffixes it))

  (defun schnouki/projectile-ag (orig-fun &rest args)
    (let ((grep-find-ignored-directories (copy-sequence grep-find-ignored-directories)))
      (apply orig-fun args)))
  (advice-add 'projectile-ag :around #'schnouki/projectile-ag)
  ;; (advice-remove 'projectile-ag #'schnouki/projectile-ag)

  (projectile-mode 1))

(use-package ibuffer-projectile
  :ensure t
  :config
  (defun schnouki/enable-ibuffer-projectile ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  (add-hook 'ibuffer-hook #'schnouki/enable-ibuffer-projectile))

;;; init-50-projectile.el ends here
