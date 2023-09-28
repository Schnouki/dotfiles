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

(with-eval-after-load-feature dap-mode
  (require 'dap-python))

(defun schnouki/fix-python-ts-hook ()
  (interactive)
  (setq-local treesit-text-type-regexp (rx (or "comment" "string_content"))))
(add-hook 'python-ts-mode-hook #'schnouki/fix-python-ts-hook)

(with-eval-after-load 'flycheck
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
    :command ("ruff"
	      "--format=text"
	      (eval (when buffer-file-name
		      (concat "--stdin-filename=" buffer-file-name)))
	      "-")
    :standard-input t
    :error-filter (lambda (errors)
		    (let ((errors (flycheck-sanitize-errors errors)))
		      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
	      (file-name) ":" line ":" (optional column ":") " "
	      (id (one-or-more (any alpha)) (one-or-more digit)) " "
	      (message (one-or-more not-newline))
	      line-end))
    :modes (python-mode python-ts-mode))

  (add-to-list 'flycheck-checkers 'python-ruff)

  (defun schnouki/flycheck-set-lsp-as-ruff-next-checker ()
    (when (flycheck-valid-checker-p 'lsp)
      (flycheck-add-next-checker 'python-ruff 'lsp)))
  (add-hook 'lsp-mode-hook #'schnouki/flycheck-set-lsp-as-ruff-next-checker)

;;   ;; Inspired by the pyright checker
;;   (defun schnouki/flycheck-ruff--parse-error (output checker buffer)
;;     "Parse RUFF errors/warning from JSON OUTPUT.
;; CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
;; the BUFFER that was checked respectively."

;;     )
;;   (flycheck-define-checker python-ruff
;;     "A fast Python linter written in Rust."
;;     :command ("ruff"
;; 	      "check"
;; 	      "--format=json"
;; 	      "--quiet"
;; 	      "--exit-zero"
;; 	      source-original)
;;     :error-patterns ()
;;     :working-directory flycheck-python-find-project-root
;;     :modes (python-mode python-ts-mode)
;;     )
  )

;;; init-40-python.el ends here
