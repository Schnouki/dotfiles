;;; 50-productivity --- Productivity stuff
;;; Commentary:
;;; Code:

(use-package pomm
  :ensure t
  :bind (:map schnouki-prefix-map
              ("t" . pomm-third-time))
  :custom
  (pomm-third-time-fraction "1/4")
  :config
  (pomm-mode-line-mode 1))

;;; init-50-productivity.el ends here
