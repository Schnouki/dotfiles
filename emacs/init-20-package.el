;; -----------------------------------------------------------------------------
;; Emacs official package manager
;; -----------------------------------------------------------------------------

;; Repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))

(package-initialize)

;; Packages
(setq schnouki/packages '(anything anything-match-plugin coffee-mode color-theme deft
			  dtrt-indent haskell-mode ioccur lua-mode magit magithub
			  markdown-mode mediawiki php-mode pretty-lambdada python-pep8
			  solarized-theme undo-tree yaml-mode zenburn-theme znc))
(let ((refreshed nil))
  (dolist (package schnouki/packages)
    (unless (package-installed-p package)
      (message (concat "Installing missing package: " (symbol-name package)))
      (unless refreshed
	(package-refresh-contents)
	(setq refreshed t))
      (package-install package))))
