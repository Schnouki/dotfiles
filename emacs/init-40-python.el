;;; 40-python --- Python-specific stuff
;;; Commentary:
;;; Code:

(use-package flycheck-local-flake8
  :load-path "flycheck-local-flake8"
  :commands flycheck-local-flake8/flycheck-virtualenv-set-python-executables
  :init
  (add-hook 'flycheck-before-syntax-check-hook
	    #'flycheck-local-flake8/flycheck-virtualenv-set-python-executables 'local))

(use-package anaconda-mode
  :ensure t
  :commands anaconda-mode
  :diminish anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode))

(use-package company-anaconda
  :ensure t
  :init
  (add-to-list 'company-backends 'company-anaconda))

(use-package auto-virtualenvwrapper
  :ensure t
  :commands auto-virtualenvwrapper-activate
  :init
  (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
  (add-hook 'window-configuration-change-hook #'auto-virtualenvwrapper-activate)
  (add-hook 'focus-in-hook #'auto-virtualenvwrapper-activate)
  :config
  (setq auto-virtualenvwrapper-verbose nil))

(use-package py-autopep8
  :ensure t
  :commands (py-autopep8-buffer)
  :init
  (defvar-local schnouki/disable-autopep8 nil
    "When set, disable autopep8 in the buffer.")
  (defun schnouki/maybe-autopep8-buffer ()
    "Uses the \"autopep8\" tool to reformat the current buffer, unless disabled."
    (interactive)
    (unless schnouki/disable-autopep8
      (py-autopep8-buffer)))
  (defun schnouki/maybe-autopep8-enable-on-save ()
    "Pre-save hook to be used before running maybe-autopep8."
    (interactive)
    (add-hook 'before-save-hook 'schnouki/maybe-autopep8-buffer nil t))
  (add-hook 'python-mode-hook 'schnouki/maybe-autopep8-enable-on-save))

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

;; Pylint for Python 2
(flycheck-define-checker python2-pylint
  "A Python syntax and style checker using Pylint2."
  :command ("pylint2" "-r" "n"
	    "--msg-template" "{path}:{line}:{column}:{C}:{symbol}/{msg_id}:{msg}"
	    (config-file "--rcfile" flycheck-pylint2rc)
	    source-inplace)
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":"
	  (or "E" "F") ":"
	  (id (one-or-more (not (any ":")))) ":"
	  (message) line-end)
   (warning line-start (file-name) ":" line ":" column ":"
	    (or "W" "R") ":"
	    (id (one-or-more (not (any ":")))) ":"
	    (message) line-end)
   (info line-start (file-name) ":" line ":" column ":"
	 "C:" (id (one-or-more (not (any ":")))) ":"
	 (message) line-end))
  :modes python-mode)
(flycheck-def-config-file-var flycheck-pylint2rc python2-pylint ".pylintrc"
  :safe #'stringp)
(list-utils-insert-after flycheck-checkers 'python-pylint 'python2-pylint)

(use-package cython-mode
  :ensure t
  :mode "\\.p\\(?:yx\\|x[di]\\)\\'")

;;; init-40-python.el ends here
