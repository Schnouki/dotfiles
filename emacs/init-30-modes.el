;;; 30-modes --- Major modes
;;; Commentary:
;;; Code:

;; Prepare various major modes
(use-package lua-mode
  :ensure lua-mode
  :mode "\\.lua\\'"
  :init
  (folding-add-to-marks-list 'lua-mode "-- {{{" "-- }}}" nil t))

(use-package go-mode
  :ensure go-mode
  :mode "\\.go\\'"
  :init
  (progn
    (folding-add-to-marks-list 'go-mode "// {{{" "// }}}" nil t)
    (defun schnouki/maybe-gofmt-before-save ()
      (when (eq major-mode 'go-mode)
	(gofmt-before-save)))
    (add-hook 'before-save-hook 'schnouki/maybe-gofmt-before-save)))

(use-package haskell-mode
  :ensure haskell-mode
  :mode "\\.hs\\'")

(use-package python
  :ensure python
  :mode ("\\.py'" . python-mode)
  :init
  (progn
    (defalias 'python2-mode 'python-mode)
    (defalias 'python3-mode 'python-mode)))

(use-package js3-mode
  :ensure js3-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :init
  (folding-add-to-marks-list 'js3-mode "// {{{" "// }}}" nil t))

(use-package coffee-mode
  :ensure coffee-mode
  :mode "\\.coffee\'"
  :init
  (progn
    (folding-add-to-marks-list 'coffee-mode "# {{{" "# }}}" nil t)
    (add-hook 'coffee-mode-hook
	      '(lambda ()
		 (setq tab-width 4
		       coffee-tab-width 4)
		 (local-set-key (kbd "C-c C-c") 'coffee-compile-buffer)))))

(use-package php-mode
  :ensure php-mode
  :mode "\\.php[345]?\\'")

(use-package yaml-mode
  :ensure yaml-mode
  :mode "\\.ya?ml\\'")

(use-package markdown-mode
  :ensure markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
  	 ("\\.mdwn\\'" . markdown-mode)
  	 ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "~/.config/emacs/markdown"))

(use-package adoc-mode
  :ensure adoc-mode
  :mode "\\.adoc?\\'")

(use-package jinja2-mode
  :ensure jinja2-mode
  :mode "\\.j2\\'")

(use-package scss-mode
  :ensure scss-mode
  :mode "\\.scss\\'"
  :init
  (setq scss-compile-at-save nil))

(use-package haml-mode
  :ensure haml-mode
  :mode "\\.haml\\'")

(use-package cuda-mode
  :mode "\\.cu\\'"
  :init
  (folding-add-to-marks-list 'cuda-mode "// {{{" "// }}}" nil t))

(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

(use-package pkgbuild-mode
  :ensure pkgbuild-mode
  :mode "/PKGBUILD\\'")

(use-package gnuplot
  :mode "\\.gp\\'\\|\\.plot\\'"
  :commands gnuplot-make-buffer)

(use-package plantuml-mode
  :commands plantuml-mode)

(use-package es-mode
  :ensure es-mode
  :commands es-mode)

(use-package po-mode
  :mode "\\.pot?\\'")
(use-package po-compat
  :commands po-find-file-coding-system
  :init
  (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\." 'po-find-file-coding-system))

;; -----------------------------------------------------------------------------
;; Minor modes
;; -----------------------------------------------------------------------------

;; HideShow minor mode for common major modes
(dolist (hook '(c-mode-common-hook emacs-lisp-mode-hook java-mode-hook lisp-mode-hook
		lua-mode perl-mode-hook python-mode sh-mode-hook))
  (add-hook hook 'hs-minor-mode))

;; smerge-mode, as suggested in the doc
(use-package smerge-mode
  :commands smerge-mode
  :init
  (progn
    (setq smerge-command-prefix (kbd "C-c '")))

    (defun sm-try-smerge ()
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "^<<<<<<< " nil t)
	  (smerge-mode 1))))
    (add-hook 'find-file-hook 'sm-try-smerge)
    (add-hook 'after-revert-hook 'sm-try-smerge))

;; geiser, for Scheme REPL and autodoc
(use-package geiser
  :ensure geiser)

;;; init-30-modes.el ends here
