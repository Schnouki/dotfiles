;;; 40-python --- Python-specific stuff
;;; Commentary:
;;; Code:


(use-package auto-virtualenvwrapper
  :ensure t
  :commands auto-virtualenvwrapper-activate
  :hook ((python-mode window-configuration-change) . auto-virtualenvwrapper-activate)
  :init
  (setq auto-virtualenvwrapper-verbose nil)
  (defun schnouki/ava-on-focus-change ()
    (when (frame-focus-state)
      (auto-virtualenvwrapper-activate)))
  (add-function :before after-focus-change-function #'schnouki/ava-on-focus-change))


(use-package cython-mode
  :ensure t
  :mode "\\.p\\(?:yx\\|x[di]\\)\\'")


;;; init-40-python.el ends here
