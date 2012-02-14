;; -----------------------------------------------------------------------------
;; Emacs official package manager
;; -----------------------------------------------------------------------------

;; Repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))

(package-initialize)

;; Packages
(setq schnouki/packages '(coffee-mode deft dtrt-indent haskell-mode ioccur mediawiki
			  pymacs python-mode solarized-theme zenburn-theme znc))
(let ((refreshed nil))
  (dolist (package schnouki/packages)
    (unless (package-installed-p package)
      (message (concat "Installing missing package: " (symbol-name package)))
      (unless refreshed
	(package-refresh-contents)
	(setq refreshed t))
      (package-install package))))
