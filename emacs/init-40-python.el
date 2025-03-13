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

(use-package indent-bars
  :ensure t
  :hook ((python-mode yaml-mode) . indent-bars-mode)
  :custom
  (indent-bars-color '(shadow :face-bg nil :blend 0.325))
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 0.8))
  (indent-bars-highlight-current-depth '(:blend 0.65))
  (indent-bars-pattern ".")
  (indent-bars-treesit-support t)
  (indent-bars-width-frac 0.1))

;; (indent-bars-reset-styles)

;;; init-40-python.el ends here
