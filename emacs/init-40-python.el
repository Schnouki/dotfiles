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


;; Configure basedpyright in eglot, based on pyright
(with-eval-after-load-feature 'eglot
  (when (executable-find "basedpyright-langserver")
    (add-to-list 'eglot-server-programs
                 '((python-mode python-ts-mode)
                   "basedpyright-langserver" "--stdio"))))

;;; init-40-python.el ends here
