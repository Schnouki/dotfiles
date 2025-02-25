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
  (pomm-mode-line-mode 1)

  ;; Expose modeline over D-Bus
  (defun schnouki/update-pomm-mode-line-dbus-property (&rest r)
    (dbus-register-property :session dbus-service-emacs "/net/schnouki/pomm" "net.schnouki.pomm" "mode-line" :read pomm-current-mode-line-string t))
  (advice-add #'pomm-third-time-update-mode-string :after #'schnouki/update-pomm-mode-line-dbus-property))

;;; init-50-productivity.el ends here
