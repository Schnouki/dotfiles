;;; 30-modes --- Major modes
;;; Commentary:
;;; Code:

;; Prepare various major modes
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :init
  (folding-add-to-marks-list 'lua-mode "-- {{{" "-- }}}" nil t))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :commands (godoc gofmt gofmt-before-save)
  :init
  (progn
    (folding-add-to-marks-list 'go-mode "// {{{" "// }}}" nil t)
    (defun schnouki/maybe-gofmt-before-save ()
      (when (eq major-mode 'go-mode)
	(gofmt-before-save)))
    (add-hook 'before-save-hook 'schnouki/maybe-gofmt-before-save)

    ;; From https://github.com/bradleywright/emacs.d
    ;; Update GOPATH if there's a _vendor (gom) or vendor (gb) dir
    (defun schnouki/set-local-go-path ()
      "Sets a local GOPATH if appropriate"
      (let ((current-go-path (getenv "GOPATH")))
        (catch 'found
          (dolist (vendor-dir '("_vendor" "vendor"))
            (let ((directory (locate-dominating-file (buffer-file-name) vendor-dir)))
              (when directory
                (make-local-variable 'process-environment)
                (let ((local-go-path (concat (expand-file-name directory) vendor-dir)))
                  (if (not current-go-path)
                      (setenv "GOPATH" local-go-path)
                    (unless (string-match-p local-go-path current-go-path)
                      (setenv "GOPATH" (concat local-go-path ":" current-go-path))))
                  (setq-local go-command
                              (concat "GOPATH=\"" local-go-path ":" (expand-file-name directory) ":${GOPATH}\" " go-command))
                  (throw 'found local-go-path))))))))
    (add-hook 'go-mode-hook 'schnouki/set-local-go-path))
  :config
  (progn
    ;; http://yousefourabi.com/blog/2014/05/emacs-for-go/
    (bind-key "C-c C-f" 'gofmt go-mode-map)
    (bind-key "C-c C-g" 'go-goto-imports go-mode-map)
    (bind-key "C-c C-k" 'godoc go-mode-map)
    (bind-key "C-c C-r" 'go-remove-unused-imports go-mode-map)))

(use-package company-go
  :ensure t
  :commands company-go
  :init (add-to-list 'company-backends 'company-go))

(use-package go-eldoc
  :ensure t
  :commands go-eldoc-setup
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'\\|\\.gradle\\'")

(use-package python
  :mode ("\\.py'" . python-mode)
  :init
  (progn
    (defalias 'python2-mode 'python-mode)
    (defalias 'python3-mode 'python-mode)))

(use-package hy-mode
  :ensure t
  :mode "\\.hy\\'")

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :init
  (folding-add-to-marks-list 'js2-mode "// {{{" "// }}}" nil t))

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\'"
  :init
  (progn
    (folding-add-to-marks-list 'coffee-mode "# {{{" "# }}}" nil t)
    (add-hook 'coffee-mode-hook
	      '(lambda ()
		 (setq tab-width 4
		       coffee-tab-width 4)
		 (local-set-key (kbd "C-c C-c") 'coffee-compile-buffer)))))

(use-package actionscript-mode
  :ensure t
  :mode "\\.as\'")

(use-package php-mode
  :ensure t
  :mode "\\.php[345]?\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
  	 ("\\.mdwn\\'" . markdown-mode)
  	 ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "~/.config/emacs/markdown"))

(use-package adoc-mode
  :ensure t
  :mode "\\.adoc?\\'")

(use-package jinja2-mode
  :ensure t
  :mode "\\.j2\\'")

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :init
  (setq scss-compile-at-save nil))

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'"
  :config
  (add-to-list 'haml-block-openers
               "^[ \t]*-[ \t]*elif"))

(use-package cuda-mode
  :mode "\\.cu\\'"
  :init
  (folding-add-to-marks-list 'cuda-mode "// {{{" "// }}}" nil t))

(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

(use-package vala-mode
  :ensure t
  :mode "\\.vala\\'")

(use-package pkgbuild-mode
  :ensure t
  :mode "/PKGBUILD\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "/Dockerfile\\'")

(use-package puppet-mode
  :ensure t
  :mode "\\.pp\\'")

(use-package gnuplot
  :mode "\\.gp\\'\\|\\.plot\\'"
  :commands gnuplot-make-buffer)

(use-package graphviz-dot-mode
  :ensure t
  :mode "\\.dot\\'")

(use-package es-mode
  :ensure t
  :commands es-mode)

(use-package po-mode
  :mode "\\.pot?\\'")
(use-package po-compat
  :commands po-find-file-coding-system
  :init
  (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\." 'po-find-file-coding-system))

(use-package bats-mode
  :ensure t
  :mode "\\.bats\\'")

(use-package systemd
  :ensure t
  :mode ("\\.automount\\'\\|\\.busname\\'\\|\\.mount\\'\\|\\.service\\'\\|\\.slice\\'\\|\\.socket\\'\\|\\.target\\'\\|\\.timer\\'\\|\\.link\\'\\|\\.netdev\\'\\|\\.network\\'\\|\\.override\\.conf.*\\'" . systemd-mode))

;; -----------------------------------------------------------------------------
;; Minor modes
;; -----------------------------------------------------------------------------

;; HideShow minor mode for common major modes
(dolist (hook '(c-mode-common-hook emacs-lisp-mode-hook java-mode-hook lisp-mode-hook
		lua-mode perl-mode-hook python-mode-hook sh-mode-hook))
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
  :ensure t)

;; eldoc-mode
(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook python-mode-hook))
  (add-hook hook 'eldoc-mode))

;;; init-30-modes.el ends here
