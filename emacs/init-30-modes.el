;;; 30-modes --- Major modes
;;; Commentary:
;;; Code:

;; Prepare various major modes
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")
(use-package fennel-mode
  :ensure t)

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'\\|\\.gradle\\'")

(use-package kotlin-mode
  :ensure t
  :mode "\\.kt\\'")

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
  :interpreter "node")

(use-package js-doc
  :ensure t
  :bind (:map js2-mode-map
              ;("C-c C-@" . js-doc-describe-tag)
              ("C-c i"   . js-doc-insert-function-doc)
              ("@"       . js-doc-insert-tag)))

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\'"
  :init
  (add-hook 'coffee-mode-hook
	    '(lambda ()
	       (setq tab-width 4
		     coffee-tab-width 4)
	       (local-set-key (kbd "C-c C-c") 'coffee-compile-buffer))))

(use-package tide
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (add-hook 'typescript-mode-hook
            (lambda ()
              (tide-setup)
              (add-hook 'before-save-hook 'tide-format-before-save nil t))
            ))

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

(use-package sass-mode
  :ensure t
  :mode "\\.sass\\'")

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :init
  (setq scss-compile-at-save nil))

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'"
  :init
  (setq less-css-compile-at-save nil))

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'"
  :config

  (setq schnouki/hamlpy-keywords '("elif" "block" "blocktrans" "with"))

  ;; Reset haml-block-openers -- inspired by http://emacs.stackexchange.com/a/10809/2006
  ;; TODO: make a nice function out of this :)
  (save-window-excursion
    (save-excursion
      (find-variable 'haml-block-openers)
      (eval-defun nil)))
  (add-to-list 'haml-block-openers
               (concat "^[ \t]*[&!]?[-=~][ \t]*\\("
                       (regexp-opt schnouki/hamlpy-keywords)
                       "\\)"))

  (defun schnouki/haml-fontify-region-as-ruby (beg end)
    "Use Ruby's font-lock variables to fontify the region between BEG and END."
    (let ((keywords ruby-font-lock-keywords))
      (add-to-list 'ruby-font-lock-keywords
                   `(,(concat ruby-font-lock-keyword-beg-re
                              (regexp-opt schnouki/hamlpy-keywords
                                          'symbols))
                     (1 font-lock-keyword-face)))
      (haml-fontify-region beg end keywords
                           ruby-font-lock-syntax-table
                            (if (fboundp 'ruby-syntax-propertize)
                                'ruby-syntax-propertize
                              'ruby-syntax-propertize-function))))
  (advice-add 'haml-fontify-region-as-ruby :override #'schnouki/haml-fontify-region-as-ruby))

;; (use-package handlebars-mode
;;   :ensure t
;;   :mode "\\.hbs\\'")

(use-package web-mode
  :ensure t
  ;; (kill-new (format "\"%s\"" (s-replace "\\" "\\\\" (rx "." (or "tmpl" "hbs" "html" "liquid" "mako") eos))))
  :mode "\\.\\(?:\\(?:h\\(?:bs\\|tml\\)\\|liquid\\|mako\\|tmpl\\)\\)\\'"
  :init
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")
				 ("go" . "\\.tmpl\\'")
				 ("mako" . "\\.mako\\'"))))

(use-package cuda-mode
  :mode "\\.cu\\'")

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

(use-package plantuml-mode
  :ensure t
  :mode "\\.plu\\'"
  :custom
  (plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  (plantuml-default-exec-mode 'jar))

(use-package flycheck-plantuml
  :ensure t
  :commands (flycheck-plantuml-setup)
  :init
  (with-eval-after-load 'flycheck
    (flycheck-plantuml-setup)))

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package elixir-mode
  :ensure t
  :mode "\\.exs?\\'")

(use-package caddyfile-mode
  :load-path "~/dev/caddyfile-mode"
  :mode (("Caddyfile\\'" . caddyfile-mode)
	 ("caddy\\.conf\\'" . caddyfile-mode)))

(use-package d-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :custom
  (rust-format-on-save t))
(use-package cargo
  :ensure t)

(use-package fish-mode
  :ensure t
  :commands (fish-mode fish_indent-before-save)
  :init
  (add-hook 'fish-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'fish_indent-before-save))))

(use-package qml-mode
  :ensure t)

(use-package nim-mode
  :ensure t)
(use-package flycheck-nim
  :ensure t)

(use-package scad-mode
  :ensure t)

;; -----------------------------------------------------------------------------
;; Minor modes
;; -----------------------------------------------------------------------------

;; smerge-mode, as suggested in the doc
(use-package smerge-mode
  :commands smerge-mode
  :bind ("C-c '" . hydra-smerge/body)
  :init
  (defun schnouki/maybe-enable-smerge ()
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
	(smerge-mode 1))))
  (add-hook 'find-file-hook 'schnouki/maybe-enable-smerge)
  (add-hook 'after-revert-hook 'schnouki/maybe-enable-smerge)

  :config
  (defhydra hydra-smerge (:hint nil
			  :pre (smerge-mode 1)
			  :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper (mine)       _=_: upper/lower       _r_esolve
^^           _l_ower (other)      _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("q" nil "cancel" :color blue)))


;; geiser, for Scheme REPL and autodoc
(use-package geiser
  :ensure t)

;; eldoc-mode
(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook python-mode-hook))
  (add-hook hook 'eldoc-mode))

;; Format JSON / JSONlines with JQ
(use-package jq-format
  :ensure t)

;;; init-30-modes.el ends here
