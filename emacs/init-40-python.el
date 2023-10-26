;;; 40-python --- Python-specific stuff
;;; Commentary:
;;; Code:


(use-package auto-virtualenvwrapper
  :ensure t
  :commands auto-virtualenvwrapper-activate
  :hook ((python-mode window-configuration-change focus-in) . auto-virtualenvwrapper-activate)
  :custom
  (auto-virtualenvwrapper-verbose nil))


(use-package cython-mode
  :ensure t
  :mode "\\.p\\(?:yx\\|x[di]\\)\\'")


;;; init-40-python.el ends here
