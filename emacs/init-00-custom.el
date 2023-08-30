(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-virtualenvwrapper-verbose nil t nil "Customized with use-package auto-virtualenvwrapper")
 '(company-emoji-insert-unicode nil)
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "kubernetes")
      tramp-kubernetes-connection-local-default-profile)
     ((:application eshell) eshell-connection-default-profile)
     ((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile
      tramp-flatpak-connection-local-default-profile)
     ((:application tramp) tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-flatpak-connection-local-default-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin"
			 "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin"
			 "/local/bin" "/local/freeware/bin" "/local/gnu/bin"
			 "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
			 "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-kubernetes-connection-local-default-profile
      (tramp-config-check . tramp-kubernetes--current-context-data)
      (tramp-extra-expand-args 97
			       (tramp-kubernetes--container
				(car tramp-current-connection))
			       104
			       (tramp-kubernetes--pod
				(car tramp-current-connection))
			       120
			       (tramp-kubernetes--context-namespace
				(car tramp-current-connection))))
     (eshell-connection-default-profile (eshell-path-env-list))
     (tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin"
			 "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin"
			 "/local/bin" "/local/freeware/bin" "/local/gnu/bin"
			 "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
			 "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
					"pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
					"-o" "state=abcde" "-o"
					"ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number)
					  (user . string) (egid . number)
					  (comm . 52) (state . 5)
					  (ppid . number) (pgrp . number)
					  (sess . number) (ttname . string)
					  (tpgid . number) (minflt . number)
					  (majflt . number)
					  (time . tramp-ps-time) (pri . number)
					  (nice . number) (vsize . number)
					  (rss . number) (etime . tramp-ps-time)
					  (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
					"pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
					"-o" "stat=abcde" "-o"
					"ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number) (user . string)
					  (group . string) (comm . 52)
					  (state . 5) (ppid . number)
					  (pgrp . number) (ttname . string)
					  (time . tramp-ps-time) (nice . number)
					  (etime . tramp-ps-time) (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
					"pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
					"-o"
					"state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number)
					  (user . string) (egid . number)
					  (group . string) (comm . 52)
					  (state . string) (ppid . number)
					  (pgrp . number) (sess . number)
					  (ttname . string) (tpgid . number)
					  (minflt . number) (majflt . number)
					  (time . tramp-ps-time) (pri . number)
					  (nice . number) (vsize . number)
					  (rss . number) (etime . number)
					  (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile (shell-file-name . "/bin/sh")
						   (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile (path-separator . ":")
						    (null-device . "/dev/null"))))
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
   '(parinfer-rust-mode lsp-docker lsp-treemacs treemacs apheleia treesit-auto age
			git-modes sly-named-readtables sly-asdf vterm mastodon
			pueue adoc-mode alert auto-read-only
			auto-virtualenvwrapper avy avy-flycheck bind-key
			browse-kill-ring caddyfile-mode cargo cmake-mode
			coffee-mode company company-org-block company-tabnine
			counsel crossword cython-mode dash deadgrep defproject
			dfmt diminish dired-collapse dired-subtree d-mode
			dockerfile-mode dtrt-indent dumb-jump editorconfig
			elixir-mode emojify evil-numbers expand-region faceup
			fennel-mode fish-mode flycheck flycheck-mypy
			flycheck-nim flycheck-package flycheck-plantuml
			flycheck-pos-tip flyspell gitignore-mode git-link
			go-add-tags godoctor go-eldoc go-guru go-mode
			google-translate go-rename goto-last-change
			graphviz-dot-mode guess-language helpful hl-todo hydra
			hy-mode ibuffer-projectile ivy ix jinja2-mode jq-mode
			js2-mode js-doc just-mode less-css-mode ligature
			list-utils loop lsp-mode lsp-pyright lsp-ui lua-mode
			magit memory-usage move-text multiple-cursors nim-mode
			notmuch org org-contrib org-mime ox-gfm ox-md
			package-lint php-mode pkgbuild-mode plantuml-mode
			po-compat po-mode projectile python rainbow-mode
			restclient restclient-jq ripgrep rust-mode s sass-mode
			scad-mode scss-mode shrink-whitespace sly-quicklisp
			smart-comment smart-forward smart-mode-line smerge-mode
			sudoku swiper systemd tide toml-mode transpose-frame
			unicode-troll-stopper use-package uuidgen viking-mode
			visual-fill-column volatile-highlights wgrep
			writeroom-mode wttrin xref yasnippet zenburn-theme) nil nil "COUCOU!")
 '(safe-local-variable-values
   '((eval add-to-local-list 'yas-snippet-dirs "/home/schnouki/doist/snippets")
     (lsp-pyright-extra-paths . ["/home/schnouki/doist/ist_libs/python"])
     (python-fill-docstring-style . python-257-nn) (blacken-mode . t)
     (python-fill-docstring-style . django) (eval blacken-mode t)
     (company-tabnine-always-trigger) (schnouki/disable-autopep8 . t)
     (py-autopep8-options quote ("--max-line-length=100"))
     (py-autopep8-options "--max-line-length=100")
     (plantuml-output-type . "svg") (org-image-actual-width . 120)
     (origami-fold-style . triple-braces)
     (eval when (require 'rainbow-mode nil t) (rainbow-mode 1))
     (eval when (require 'rainbow-mode nil t) (schnouki/rainbow-mode-zenburn))
     (js2-basic-offset . 4) (nxml-child-indent . 2) (nxml-attribute-indent . 4)
     (lua-indent-level . 4) (org-enable-table-editor) (nxml-child-indent . 8)
     (nxml-attribute-indent . 8) (require-final-newline)
     (ack-and-a-half-prompt-for-directory . t)
     (flycheck-checker . python2-pylint) (delete-trailing-lines)
     (js3-strict-trailing-comma-warning) (js2-strict-trailing-comma-warning)
     (haml-indent-offset . 4) (encoding . utf-8) (pkgbuild-update-sums-on-save)
     (py-indent-offset . 4) (ispell-dictionary . "francais")
     (ispell-dictionary . "american")))
 '(sudoku-download t nil nil "Customized with use-package sudoku")
 '(sudoku-level 'medium nil nil "Customized with use-package sudoku")
 '(sudoku-style 'unicode nil nil "Customized with use-package sudoku")
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
