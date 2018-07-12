(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t)
 '(ecb-options-version "2.40")
 '(inhibit-startup-screen t)
 '(magit-fetch-arguments (quote ("--prune")))
 '(magit-log-section-arguments (quote ("--decorate" "-n256")))
 '(package-selected-packages
   (quote
    (rg ripgrep magit-todos hl-todo blacken cython-mode orgtbl-aggregate multiple-cursors diminish helpful emojify-logos counsel swiper ivy auto-virtualenvwrapper pomidor less-css-mode less-mode dired-collapse dired-subtree py-autopep8 ido-completing-read+ amx move-text browse-kill-ring ibuffer-projectile auto-read-only dumb-jump dump-jump editorconfig list-utils which-key flycheck-plantuml guess-language toml-mode godoctor js-doc wgrep origami avy-flycheck go-add-tags sass-mode flycheck-ledger go-rename go-guru web-mode plantuml-mode handlebars-mode kotlin-mode tide wttrin viking-mode volatile-highlights ggtags projectile flx-ido oracle zenburn-theme yasnippet yaml-mode websocket vala-mode use-package unicode-troll-stopper undo-tree unbound twittering-mode systemd sudoku smart-mode-line smart-forward shrink-whitespace scss-mode restclient request rainbow-mode python-environment puppet-mode prodigy popup pkgbuild-mode php-mode org oauth2 nose markdown-mode magit-gitflow magit-annex lua-mode json-rpc js2-mode jinja2-mode ix iedit ido-vertical-mode ido-clever-match hydra hy-mode highlight-indentation haskell-mode haml-mode groovy-mode graphviz-dot-mode goto-last-change google-translate google-this go-eldoc gitignore-mode git-annex geiser fuzzy fringe-helper flycheck-pos-tip evil-numbers es-mode erlang epc engine-mode emojify dtrt-indent dockerfile-mode deft dash-functional ctags-update company-go company-emoji company-anaconda color-theme coffee-mode bonjourmadame bats-mode avy-zap auctex alert adoc-mode actionscript-mode smart-comment ido-occur)))
 '(safe-local-variable-values
   (quote
    ((schnouki/disable-autopep8 . t)
     (py-autopep8-options quote
			  ("--max-line-length=100"))
     (py-autopep8-options "--max-line-length=100")
     (plantuml-output-type . "svg")
     (org-image-actual-width . 120)
     (origami-fold-style . triple-braces)
     (eval when
	   (require
	    (quote rainbow-mode)
	    nil t)
	   (rainbow-mode 1))
     (eval when
	   (require
	    (quote rainbow-mode)
	    nil t)
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
     (ispell-dictionary . "american")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
