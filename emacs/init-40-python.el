;;; 40-python --- Python-specific stuff
;;; Commentary:
;;; Code:

(use-package virtualenvwrapper
  :ensure virtualenvwrapper
  :commands (venv-workon venv-deactivate venv-initialize-interactive-shells venv-initialize-eshell)
  :config
  (message "virtualenvwrapper loaded")
  :init
  (progn
    (setq venv-location "~/.virtualenvs")

    (defcustom python-venv nil
      "Name of the virtualenv to work on."
      :group 'python
      :safe #'stringp)(boundp 'python-venv)

    (defun schnouki/setup-venv ()
      (hack-local-variables)
      (when python-venv
	(venv-workon python-venv)))
    (add-hook 'python-mode-hook 'schnouki/setup-venv)))

(use-package jedi
  :ensure jedi
  :commands jedi:setup
  :init
  (progn
    (setq jedi:complete-on-dot t)
    (add-hook 'python-mode-hook 'jedi:setup)))

;;; init-40-python.el ends here
