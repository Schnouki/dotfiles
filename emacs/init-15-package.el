;;; 15-package --- Emacs official package manager
;;; Commentary:
;;; Code:

(require 'package)

;; Repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; Packages
(defvar schnouki/packages nil)
(setq schnouki/packages '(ace-jump-mode ack-and-a-half auto-complete
			  button-lock
			  clippy coffee-mode color-theme csharp-mode
			  deft dtrt-indent
			  fixmee flycheck
			  git-annex git-commit-mode git-gutter-fringe gitignore-mode go-mode google-translate
			  haml-mode haskell-mode hideshowvis
			  ioccur indent-guide
			  jabber jedi jquery-doc js3-mode
			  lua-mode
			  magit markdown-mode mediawiki melpa multiple-cursors mc-extras
			  php-mode pkgbuild-mode pretty-lambdada prodigy python python-pep8
			  rainbow-mode
			  scss-mode sudoku
			  twittering-mode
			  unbound undo-tree
			  virtualenv
			  yaml-mode yasnippet
			  zenburn-theme))
(let ((refreshed nil))
  (dolist (package schnouki/packages)
    (unless (package-installed-p package)
      (message (concat "Installing missing package: " (symbol-name package)))
      (unless refreshed
	(package-refresh-contents)
	(setq refreshed t))
      (package-install package))))

;;; init-15-package.el ends here
