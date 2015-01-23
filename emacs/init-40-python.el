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
      :safe #'stringp)

    (defun schnouki/setup-venv ()
      (hack-local-variables)
      (when python-venv
	(venv-workon python-venv)))
    (add-hook 'python-mode-hook 'schnouki/setup-venv)))

(use-package anaconda-mode
  :ensure anaconda-mode
  :commands anaconda-mode
  :diminish anaconda-mode
  :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'eldoc-mode)))

(use-package company-anaconda
  :ensure company-anaconda
  :init (add-to-list 'company-backends 'company-anaconda))

(use-package company-inf-python
  :ensure company-inf-python
  :init (add-to-list 'company-backends 'company-inf-python))

;; Django helper
(defun schnouki/use-django-interactive-shell ()
  "Auto-detect Django projects and change the interactive shell to `manage.py shell'."
  (interactive)
  (let ((file (buffer-file-name))
	(found nil))
    (while (not (or (string= file "/")
		    found))
      (let* ((parent-dir (file-name-directory file))
	     (manage-py (concat parent-dir "manage.py")))
	(setq file (directory-file-name parent-dir))
	(when (file-exists-p manage-py)
	  (setq-local python-shell-interpreter-args (concat manage-py " shell"))
	  (setq found t))))))
(add-hook 'python-mode-hook 'schnouki/use-django-interactive-shell)

;;; init-40-python.el ends here
