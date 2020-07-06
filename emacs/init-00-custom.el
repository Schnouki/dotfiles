(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alert-default-style 'libnotify)
 '(auto-virtualenvwrapper-verbose nil t)
 '(calendar-date-style 'european)
 '(calendar-week-start-day 1)
 '(company-emoji-insert-unicode nil)
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 3)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 30)
 '(counsel-find-file-ignore-regexp "\\`[#.]\\|[#~]\\'\\|\\.pyc\\'")
 '(custom-safe-themes t)
 '(diary-file "~/Dropbox/org/diary")
 '(ecb-options-version "2.40")
 '(emojify-point-entered-behaviour 'uncover)
 '(forge-add-pullreq-refspec 'ask)
 '(guess-language-languages '(en fr))
 '(guess-language-min-paragraph-length 40)
 '(inhibit-startup-screen t)
 '(ivy-magic-tilde nil)
 '(ivy-re-builders-alist '((t . ivy--regex-ignore-order)) t)
 '(ivy-use-virtual-buffers t)
 '(lsp-auto-guess-root t)
 '(lsp-clients-emmy-lua-jar-path "/usr/lib/lua-emmy-language-server/EmmyLua-LS-all.jar")
 '(lsp-prefer-flymake nil t)
 '(lsp-ui-doc-alignment 'window)
 '(lsp-ui-doc-delay 0.2)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-flycheck t t)
 '(magit-completing-read-function 'ivy-completing-read)
 '(magit-fetch-arguments '("--prune"))
 '(magit-log-section-arguments '("--decorate" "-n256"))
 '(org-checkbox-hierarchical-statistics nil)
 '(org-cycle-separator-lines 3)
 '(org-directory "~/Dropbox/org/")
 '(org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
 '(org-roam-capture-templates
   '(("d" "default" plain #'org-roam-capture--get-point "%?" :file-name "${slug}" :head "#+TITLE: ${title}
" :unnarrowed t :immediate-finish t)) t)
 '(org-roam-dailies-capture-templates
   '(("w" "weekly" plain #'org-roam-capture--get-point "" :file-name "%<%Y-w%W>" :head "#+TITLE: %<%Y> semaine %<%W> (%<%Y-%m-%d>)
#+OPTIONS: toc:nil

* Meetings
* Posts
* Snippets
* Capture" :immediate-finish t)) t)
 '(org-roam-directory "~/Dropbox/org/roam/")
 '(org-startup-with-inline-images t)
 '(org-support-shift-select t)
 '(package-selected-packages
   '(fennel-mode scad-mode nord-theme flycheck-nim nim-mode deft org-roam org-brain activitywatch flycheck-mypy qml-mode docker-tramp fish-mode transpose-frame ox-gfm defproject actionscript-mode adoc-mode alert auctex auto-read-only auto-virtualenvwrapper avy avy-flycheck avy-zap bats-mode bibtex bind-key blacken bonjourmadame browse-kill-ring caddyfile-mode cargo cmake-mode coffee-mode company company-lsp counsel cuda-mode cython-mode dash deadgrep diminish dired-collapse dired-subtree d-mode dockerfile-mode dtrt-indent dumb-jump editorconfig elixir-mode emojify emojify-logos es-mode evil-numbers expand-region faceup flycheck flycheck-local-flake8 flycheck-package flycheck-plantuml flycheck-pos-tip flyspell forge geiser ggtags gitignore-mode gnuplot go-add-tags godoctor go-eldoc go-guru go-mode google-translate go-rename goto-last-change graphviz-dot-mode groovy-mode guess-language haml-mode haskell-mode helpful hl-todo hydra hy-mode ibuffer-projectile ivy ix jinja2-mode jq-format js2-mode js-doc kotlin-mode less-css-mode list-utils lsp-mode lsp-ui lua-mode magit markdown-mode memory-usage move-text multiple-cursors notmuch org origami package-lint php-mode pkgbuild-mode plantuml-mode po-compat pomidor po-mode preview projectile puppet-mode python rainbow-mode reftex restclient rg ripgrep rust-mode s sass-mode scss-mode shrink-whitespace smart-comment smart-forward smart-mode-line smerge-mode sudoku swiper systemd tide todoist toml-mode unbound undo-tree unicode-troll-stopper use-package uuidgen vala-mode viking-mode volatile-highlights web-mode wgrep which-key writeroom-mode wttrin yaml-mode yasnippet zenburn-theme))
 '(plantuml-default-exec-mode 'jar)
 '(plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
 '(projectile-completion-system 'ivy)
 '(rust-format-on-save t)
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
 '(sudoku-download t t)
 '(sudoku-level 'medium t)
 '(sudoku-style 'unicode t)
 '(wgrep-auto-save-buffer t t)
 '(which-key-max-description-length nil)
 '(which-key-show-docstrings t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
