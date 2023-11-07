;;; 30-modes --- Major modes
;;; Commentary:
;;; Code:

;; Prepare various major modes
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")
(use-package fennel-mode
  :ensure t)

(use-package python
  :mode ("\\.py'" . python-ts-mode))

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
              ;;("C-c C-@" . js-doc-describe-tag)
              ("C-c i"   . js-doc-insert-function-doc)
              ("@"       . js-doc-insert-tag))
  :custom
  (js2-ignored-warnings '("msg.missing.semi")))

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
              (add-hook 'before-save-hook 'tide-format-before-save nil t))))


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

(use-package web-mode
  :ensure t
  ;; (kill-new (format "\"%s\"" (s-replace "\\" "\\\\" (rx "." (or "tmpl" "hbs" "html" "liquid" "mako") eos))))
  :mode "\\.\\(?:\\(?:h\\(?:bs\\|tml\\)\\|liquid\\|mako\\|tmpl\\)\\)\\'"
  :init
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")
                                 ("go" . "\\.tmpl\\'")
                                 ("mako" . "\\.mako\\'"))))

(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package pkgbuild-mode
  :ensure t
  :mode "/PKGBUILD\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "/Dockerfile\\'")

(use-package graphviz-dot-mode
  :ensure t
  :mode "\\.dot\\'")

(use-package po-mode
  :mode "\\.pot?\\'")
(use-package po-compat
  :commands po-find-file-coding-system
  :init
  (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\." 'po-find-file-coding-system))

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
(use-package loop :ensure t)

(use-package d-mode
  :ensure t)
(use-package dfmt
  :ensure t
  :hook (d-mode . dfmt-setup-keys))

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

(use-package nim-mode
  :ensure t
  :hook (nim-mode . nimsuggest-mode)
  :custom
  (nimsuggest-path "~/.local/share/nimble/bin/nimsuggest"))

(use-package scad-mode
  :ensure t)

(use-package jq-mode
  :ensure t)

(use-package nix-mode
  :ensure t)

(use-package sly
  :ensure t
  :config
  ;; install it with "ros install sly"
  (load (expand-file-name "~/.roswell/helper.el"))
  (sly-setup))
(use-package sly-asdf
  :ensure t)
(use-package sly-named-readtables
  :ensure t)
(use-package sly-quicklisp
  :ensure t)

(use-package geiser
  :ensure t)
(use-package geiser-chicken
  :ensure t
  :custom
  (geiser-chicken-binary "chicken-csi"))

(use-package just-mode
  :ensure t)

(use-package janet-ts-mode
  :load-path "~/.config/emacs/janet-ts-mode")
(use-package ajrepl
  :load-path "~/.config/emacs/ajrepl")

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

;; Code formatting in many languages
(use-package apheleia
  :ensure t
  :delight
  :init
  (apheleia-global-mode +1)
  :config
  ;; Nimpretty
  (setf (alist-get 'nimpretty apheleia-formatters)
        '("~/.local/share/nimble/bin/nimpretty" inplace))
  (setf (alist-get 'nim-mode apheleia-mode-alist) '(nimpretty)))

;;; init-30-modes.el ends here
