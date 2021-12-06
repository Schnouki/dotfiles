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
   '(slime tree-sitter-langs tree-sitter loop nix-mode company-tabnine company-org-block git-link crossword flycheck-grammarly lsp-treemacs treemacs-projectile treemacs lsp-pyright ligature jq-mode fennel-mode scad-mode flycheck-nim nim-mode deft activitywatch flycheck-mypy qml-mode docker-tramp fish-mode transpose-frame ox-gfm defproject adoc-mode alert auto-read-only auto-virtualenvwrapper avy avy-flycheck avy-zap bibtex bind-key blacken bonjourmadame browse-kill-ring caddyfile-mode cargo cmake-mode coffee-mode company company-lsp counsel cuda-mode cython-mode dash deadgrep diminish dired-collapse dired-subtree d-mode dockerfile-mode dtrt-indent dumb-jump editorconfig elixir-mode emojify evil-numbers expand-region faceup flycheck flycheck-local-flake8 flycheck-package flycheck-plantuml flycheck-pos-tip flyspell forge gitignore-mode gnuplot go-add-tags godoctor go-eldoc go-guru go-mode google-translate go-rename goto-last-change graphviz-dot-mode guess-language helpful hl-todo hydra hy-mode ibuffer-projectile ivy ix jinja2-mode jq-format js2-mode js-doc less-css-mode list-utils lsp-mode lsp-ui lua-mode magit markdown-mode memory-usage move-text multiple-cursors notmuch org package-lint php-mode pkgbuild-mode plantuml-mode po-compat po-mode preview projectile python rainbow-mode reftex ripgrep rust-mode s sass-mode scss-mode shrink-whitespace smart-comment smart-forward smerge-mode sudoku systemd todoist toml-mode unbound undo-tree unicode-troll-stopper use-package uuidgen viking-mode volatile-highlights wgrep wttrin yaml-mode yasnippet))
 '(safe-local-variable-values
   '((blacken-mode . t)
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
