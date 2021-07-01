;;; 60-treemacs --- Treemacs config
;;; Commentary:
;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :bind (:map global-map
	 ("M-0" . treemacs-select-window)
	 :map schnouki-prefix-map
	 ("t t" . treemacs)
	 ("t 1" . treemacs-delete-other-windows)
	 ("t B" . treemacs-bookmark)
	 ("t C-t" . treemacs-find-file)
	 ("t M-t" . treemacs-find-tag))

  :custom
  (treemacs-show-hidden-files nil)
  (treemacs-silent-filewatch t)
  (treemacs-silent-refresh t)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  )

(use-package treemacs-magit
  :ensure t
  :after treemacs magit)

(use-package treemacs-projectile
  :ensure t
  :after treemacs projectile)

(with-eval-after-load 'treemacs
  (defun schnouki/treemacs-ignore-python (filename absolute-path)
    (or (-contains? '("__pycache__") filename)
	(s-ends-with? ".pyc" filename)))
  (add-to-list 'treemacs-ignored-file-predicates #'schnouki/treemacs-ignore-python)

  (defun schnouki/treemacs-ignore-node (filename absolute-path)
    (or (-contains? '("node_modules" "package-lock.json") filename)
	(--any? (s-ends-with? it filename) '(".min.css" ".min.js"))))
  (add-to-list 'treemacs-ignored-file-predicates #'schnouki/treemacs-ignore-node))


;; LSP treemacs
(use-package lsp-treemacs
  :ensure t
  :after treemacs lsp
  :bind (:map schnouki-prefix-map
	 ("t s" . lsp-treemacs-symbols)
	 ("t r" . lsp-treemacs-references)
	 ("t i" . lsp-treemacs-implementations)
	 ("t h c" . lsp-treemacs-call-hierarchy)
	 ("t h t" . lsp-treemacs-type-hierarchy)))

(lsp-treemacs-sync-mode 1)

;;; init-60-treemacs.el ends here
