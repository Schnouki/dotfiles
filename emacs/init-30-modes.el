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
            #'(lambda ()
                (setq tab-width 4
                      coffee-tab-width 4)
                (local-set-key (kbd "C-c C-c") 'coffee-compile-buffer))))

(use-package tide
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (add-hook 'typescript-mode-hook
            #'(lambda ()
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

(use-package sass-mode
  :ensure t
  :mode "\\.sass\\'")

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :init
  (setq scss-compile-at-save nil))

(use-package web-mode
  :ensure t
  ;; (kill-new (format "\"%s\"" (s-replace "\\" "\\\\" (rx "." (or "tmpl" "hbs" "html" "liquid" "mako") eos))))
  :mode "\\.\\(?:\\(?:h\\(?:bs\\|tml\\)\\|liquid\\|mako\\|tmpl\\)\\)\\'"
  :init
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")
                                 ("go" . "\\.tmpl\\'")
                                 ("mako" . "\\.mako\\'"))))

(use-package pkgbuild-mode
  :ensure t
  :mode "/PKGBUILD\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "/Dockerfile\\'")

(use-package systemd
  :ensure t
  :mode ("\\.automount\\'\\|\\.busname\\'\\|\\.mount\\'\\|\\.service\\'\\|\\.slice\\'\\|\\.socket\\'\\|\\.target\\'\\|\\.timer\\'\\|\\.link\\'\\|\\.netdev\\'\\|\\.network\\'\\|\\.override\\.conf.*\\'" . systemd-mode))

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

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

(use-package jq-mode
  :ensure t)

(use-package nix-mode
  :ensure t)

(use-package sly
  :ensure t
  :config
  ;; install it with "ros install sly"
  (load (expand-file-name "~/.roswell/helper.el"))
  (setq sly-lisp-implementations
        '((ros-sbcl ("ros" "run") :coding-system utf-8-unix)
          (qlot ("qlot" "exec" "ros" "run") :coding-system utf-8-unix)))
  (sly-setup))
(use-package sly-asdf
  :ensure t)
(use-package sly-named-readtables
  :ensure t)
(use-package sly-quicklisp
  :ensure t)

(use-package just-mode
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
      (save-match-data
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode 1)))))
  (add-hook 'find-file-hook 'schnouki/maybe-enable-smerge)
  (add-hook 'after-revert-hook 'schnouki/maybe-enable-smerge)

  :config
  (defhydra hydra-smerge (:hint nil
                                :pre (smerge-mode 1))
    "
^^Move       ^^Keep               ^^Diff                 ^^Other
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
    ("E" smerge-ediff :exit t)
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
  (schnouki/custom-reset-variable 'apheleia-formatters)
  (schnouki/custom-reset-variable 'apheleia-mode-alist)
  ;; Python: use "ruff format" instead of black
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff)))

;; Show info about the block at its end
(use-package scopeline
  :ensure t
  :delight
  :hook prog-mode
  :custom
  (scopeline-min-lines 10)
  (scopeline-overlay-prefix "   # ")
  :custom-face
  (scopeline-face ((t (:height 0.8 :inherit shadow))))
  :config
  (defun schnouki/hide-scopeline-on-active-line ()
    (when scopeline-mode
      (dolist (ov scopeline--overlays)
        (overlay-put ov 'after-string
                     (propertize (overlay-get ov 'after-string)
                                 'invisible (and (>= (overlay-start ov) (pos-bol))
                                                 (<= (overlay-end ov) (pos-eol))))))))
  (defun schnouki/enable-hide-scopeline-on-active-line ()
    (add-hook 'post-command-hook #'schnouki/hide-scopeline-on-active-line nil t))

  (defun schnouki/disable-scopeline-in-smerge-mode ()
    (if smerge-mode
        (scopeline-mode -1)
      (when prog-mode
        (scopeline-mode +1))))
  :hook (scopeline-mode . schnouki/enable-hide-scopeline-on-active-line)
  :hook (smerge-mode . schnouki/disable-scopeline-in-smerge-mode))



;;; init-30-modes.el ends here
