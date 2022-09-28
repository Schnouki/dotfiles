(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-virtualenvwrapper-verbose nil t nil "Customized with use-package auto-virtualenvwrapper")
 '(company-emoji-insert-unicode nil)
 '(counsel-find-file-ignore-regexp "\\`[#.]\\|[#~]\\'\\|\\.pyc\\'" nil nil "Customized with use-package counsel")
 '(custom-safe-themes t)
 '(ecb-options-version "2.40")
 '(guess-language-languages '(en fr) nil nil "Customized with use-package guess-language")
 '(guess-language-min-paragraph-length 40 nil nil "Customized with use-package guess-language")
 '(inhibit-startup-screen t)
 '(ivy-magic-tilde nil nil nil "Customized with use-package ivy")
 '(ivy-re-builders-alist '((t . ivy--regex-ignore-order)) t nil "Customized with use-package ivy")
 '(ivy-use-virtual-buffers t nil nil "Customized with use-package ivy")
 '(lsp-ui-doc-alignment 'window nil nil "Customized with use-package lsp-ui")
 '(lsp-ui-doc-delay 0.2 nil nil "Customized with use-package lsp-ui")
 '(lsp-ui-doc-include-signature t nil nil "Customized with use-package lsp-ui")
 '(lsp-ui-doc-position 'top nil nil "Customized with use-package lsp-ui")
 '(lsp-ui-flycheck t t nil "Customized with use-package lsp-ui")
 '(magit-fetch-arguments '("--prune"))
 '(magit-log-section-arguments '("--decorate" "-n256"))
 '(package-selected-packages
   '(pueue dap-mode ace-window adoc-mode alert auto-read-only auto-virtualenvwrapper avy avy-flycheck avy-zap bind-key blacken browse-kill-ring caddyfile-mode cargo cmake-mode coffee-mode company company-org-block company-tabnine counsel crossword cython-mode dash deadgrep defproject dfmt diminish dired-collapse dired-subtree d-mode dockerfile-mode docker-tramp dtrt-indent dumb-jump editorconfig elixir-mode emojify evil-numbers expand-region faceup fennel-mode fish-mode flycheck flycheck-grammarly flycheck-mypy flycheck-nim flycheck-package flycheck-plantuml flycheck-pos-tip flyspell gitignore-mode git-link go-add-tags godoctor go-eldoc go-guru go-mode google-translate go-rename goto-last-change graphviz-dot-mode guess-language helpful hl-todo hydra hy-mode ibuffer-projectile ivy ix jinja2-mode jq-format jq-mode js2-mode js-doc just-mode less-css-mode ligature list-utils loop lsp-mode lsp-pyright lsp-treemacs lsp-ui lua-mode magit markdown-mode memory-usage move-text multiple-cursors nim-mode nix-mode notmuch org org-contrib org-mime ox-gfm ox-md package-lint php-mode pkgbuild-mode plantuml-mode po-compat po-mode projectile python rainbow-mode restclient restclient-jq ripgrep rust-mode s sass-mode scad-mode scss-mode shrink-whitespace sly sly-quicklisp smart-comment smart-forward smart-mode-line smerge-mode sudoku swiper systemd tide toml-mode transpose-frame treemacs treemacs-magit treemacs-projectile tree-sitter tree-sitter-langs undo-tree unicode-troll-stopper use-package uuidgen viking-mode visual-fill-column volatile-highlights web-mode wgrep which-key writeroom-mode wttrin xref yaml-mode yasnippet zenburn-theme) nil nil "COUCOU!")
 '(safe-local-variable-values
   '((lsp-pyright-extra-paths .
			      ["/home/schnouki/doist/ist_libs/python"])
     (lsp-pyright-venv-path . "/home/schnouki/.virtualenvs/todoist/")
     (python-fill-docstring-style . python-257-nn)
     (blacken-mode . t)
     (lsp-pyls-server-command . "/home/schnouki/doist/twist/env/bin/pyls")
     (python-fill-docstring-style . django)
     (eval blacken-mode t)
     (company-tabnine-always-trigger)
     (schnouki/disable-autopep8 . t)
     (py-autopep8-options quote
			  ("--max-line-length=100"))
     (py-autopep8-options "--max-line-length=100")
     (plantuml-output-type . "svg")
     (org-image-actual-width . 120)
     (origami-fold-style . triple-braces)
     (eval when
	   (require 'rainbow-mode nil t)
	   (rainbow-mode 1))
     (eval when
	   (require 'rainbow-mode nil t)
	   (schnouki/rainbow-mode-zenburn))
     (js2-basic-offset . 4)
     (python-venv . "findspire")
     (nxml-child-indent . 2)
     (nxml-attribute-indent . 4)
     (lua-indent-level . 4)
     (org-enable-table-editor)
     (nxml-child-indent . 8)
     (nxml-attribute-indent . 8)
     (require-final-newline)
     (ack-and-a-half-prompt-for-directory . t)
     (flycheck-checker . python2-pylint)
     (delete-trailing-lines)
     (js3-strict-trailing-comma-warning)
     (js2-strict-trailing-comma-warning)
     (haml-indent-offset . 4)
     (encoding . utf-8)
     (pkgbuild-update-sums-on-save)
     (py-indent-offset . 4)
     (ispell-dictionary . "francais")
     (ispell-dictionary . "american")
     (lsp-pyls-server-command . "/home/schnouki/.virtualenvs/todoist/bin/pyls")))
 '(sudoku-download t nil nil "Customized with use-package sudoku")
 '(sudoku-level 'medium nil nil "Customized with use-package sudoku")
 '(sudoku-style 'unicode nil nil "Customized with use-package sudoku")
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((lsp-mode)))
 '(wgrep-auto-save-buffer t nil nil "Customized with use-package wgrep"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
