;; -----------------------------------------------------------------------------
;; Emacs official package manager
;; -----------------------------------------------------------------------------

(require 'package)

;; Repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; Packages
(setq schnouki/packages '(ace-jump-mode ack-and-a-half anything anything-match-plugin auto-complete
			  button-lock
			  coffee-mode color-theme ctags-update
			  deft dtrt-indent
			  fixmee
			  go-mode google-translate
			  haskell-mode hideshowvis
			  ioccur
			  lua-mode
			  magit magithub markdown-mode mediawiki melpa
			  php-mode pkgbuild-mode pretty-lambdada python python-pep8
			  rainbow-mode
			  solarized-theme sudoku
			  unbound undo-tree
			  yaml-mode yasnippet
			  zenburn-theme znc))
(let ((refreshed nil))
  (dolist (package schnouki/packages)
    (unless (package-installed-p package)
      (message (concat "Installing missing package: " (symbol-name package)))
      (unless refreshed
	(package-refresh-contents)
	(setq refreshed t))
      (package-install package))))
