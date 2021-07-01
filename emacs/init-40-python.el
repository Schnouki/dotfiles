;;; 40-python --- Python-specific stuff
;;; Commentary:
;;; Code:

(use-package flycheck-mypy
  :ensure t)

(use-package auto-virtualenvwrapper
  :ensure t
  :commands auto-virtualenvwrapper-activate
  :hook ((python-mode window-configuration-change focus-in) . auto-virtualenvwrapper-activate)
  :custom
  (auto-virtualenvwrapper-verbose nil))

(use-package blacken
  :ensure t)

;; Django helper
(defun schnouki/use-django-interactive-shell ()
  "Auto-detect Django projects and change the interactive shell to `manage.py shell'."
  (interactive)
  (let ((file (buffer-file-name))
	(found nil))
    (while (not (or (null file)
		    (string= file "/")
		    found))
      (let* ((parent-dir (file-name-directory file))
	     (manage-py (concat parent-dir "manage.py")))
	(setq file (directory-file-name parent-dir))
	(when (file-exists-p manage-py)
	  (setq-local python-shell-interpreter-args (concat manage-py " shell"))
	  (setq found t))))))
(add-hook 'python-mode-hook 'schnouki/use-django-interactive-shell)

(use-package cython-mode
  :ensure t
  :mode "\\.p\\(?:yx\\|x[di]\\)\\'")

;; Pyright language server
(use-package lsp-pyright
  :after lsp-mode
  :ensure t
  :init
  (add-to-list 'lsp-disabled-clients 'pyls))

;;; init-40-python.el ends here
