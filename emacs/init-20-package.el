;; -----------------------------------------------------------------------------
;; Emacs official package manager
;; -----------------------------------------------------------------------------

;; Repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))

(package-initialize)

;; Packages
(setq schnouki/packages '(deft haskell-mode ioccur pymacs python-mode))
(let ((count 0)
      (msg "Missing packages: "))
  (dolist (package schnouki/packages)
    (unless (package-installed-p package)
      (setq msg (concat msg
			(if (> count 0) ", ")
			(symbol-name package))
	    count (1+ count))))
  (when (> count 0)
    (message msg)))
