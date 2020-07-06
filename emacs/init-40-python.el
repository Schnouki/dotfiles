;;; 40-python --- Python-specific stuff
;;; Commentary:
;;; Code:

(use-package flycheck-local-flake8
  :load-path "flycheck-local-flake8"
  :commands flycheck-local-flake8/flycheck-virtualenv-set-python-executables
  :init
  (defun schnouki/flycheck-local-flake8-wrapper ()
    (when (and (buffer-file-name)
	       (eq major-mode 'python-mode))
      (flycheck-local-flake8/flycheck-virtualenv-set-python-executables)))
  (add-hook 'flycheck-before-syntax-check-hook #'schnouki/flycheck-local-flake8-wrapper))

(use-package flycheck-mypy
  :ensure t)

;; (use-package anaconda-mode
;;   :ensure t
;;   :commands anaconda-mode
;;   :diminish anaconda-mode
;;   :hook python-mode)

;; (use-package company-anaconda
;;   :ensure t
;;   :init
;;   (add-to-list 'company-backends 'company-anaconda))

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
