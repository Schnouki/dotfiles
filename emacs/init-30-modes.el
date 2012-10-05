;; -----------------------------------------------------------------------------
;; Major modes
;; -----------------------------------------------------------------------------

;; Prepare various major modes
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(autoload 'lua-mode "lua-mode" "Lua mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(folding-add-to-marks-list 'lua-mode "-- {{{" "-- }}}" nil t)

(folding-add-to-marks-list 'go-mode "// {{{" "// }}}" nil t)

(autoload 'python-mode "python" "Python mode." t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(defalias 'python2-mode 'python-mode)
(defalias 'python3-mode 'python-mode)

(folding-add-to-marks-list 'coffee-mode "# {{{" "# }}}" nil t)
(add-hook 'coffee-mode-hook
	  '(lambda ()
	     (setq tab-width 4
		   coffee-tab-width 4)
	     (local-set-key (kbd "C-c C-c") 'coffee-compile-buffer)))

(autoload 'php-mode "php-mode.el" "Php mode." t)
(add-to-list 'auto-mode-alist '("\\.php[345]?$" . php-mode))

(autoload 'yaml-mode "yaml-mode.el" "Yaml mode." t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(autoload 'markdown-mode "markdown-mode.el" "Markdown mode." t)
(dolist (ext '("md" "mdwn" "markdown"))
  (add-to-list 'auto-mode-alist (cons (concat "\\." ext "$") 'markdown-mode)))
(add-hook 'markdown-mode-hook '(lambda () (setq markdown-command "~/.config/emacs/markdown")))

(autoload 'adoc-mode "adoc-mode.el" "AsciiDoc mode." t)
(add-to-list 'auto-mode-alist '("\\.adoc?$" . adoc-mode))

(autoload 'jinja-mode "jinja.el" "Jinja mode." t)
(add-to-list 'auto-mode-alist '("\\.j2$" . jinja-mode))

(autoload 'cuda-mode "cuda-mode.el" "Cuda mode." t)
(add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode))
(folding-add-to-marks-list 'cuda-mode "// {{{" "// }}}" nil t)

(autoload 'cmake-mode "cmake-mode.el" "CMake mode." t)
(dolist (name '("CMakeLists\\.txt" "\\.cmake$"))
  (add-to-list 'auto-mode-alist (cons name 'cmake-mode)))

;; pkgbuild-mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))

;; gnuplot-mode
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
(add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))
(add-to-list 'auto-mode-alist '("\\.plot$" . gnuplot-mode))

;; plantuml-mode
;; nothing into auto-mode-alist, just an autoload for the mode itself...
(autoload 'plantuml-mode "plantuml-mode" "PlantUML major mode" t)

;; Golbarg, the most awesome blog engine ever
(autoload 'golbarg-mode "golbarg.el" "Golbarg mode." t)
(autoload 'golbarg-new-draft "golbarg.el" "New Golbarg draft." t)
(setq golbarg-posts-dir "~/site/schnouki.net/posts"
      golbarg-drafts-dir "~/Dropbox/blog-drafts")
(global-set-key (kbd "C-! g") 'golbarg-new-draft)
(global-set-key (kbd "C-ç g") 'golbarg-new-draft)
(global-set-key (kbd "C-! M-g") '(lambda () (interactive) (find-file golbarg-drafts-dir)))
(global-set-key (kbd "C-ç M-g") '(lambda () (interactive) (find-file golbarg-drafts-dir)))
(add-to-list 'auto-mode-alist (cons (concat "^" (expand-file-name golbarg-posts-dir)  "/.+") 'golbarg-mode))
(add-to-list 'auto-mode-alist (cons (concat "^" (expand-file-name golbarg-drafts-dir) "/.+") 'golbarg-mode))
(add-hook 'golbarg-mode-hook
	  '(lambda ()
	     (turn-on-auto-fill)
	     (ispell-change-dictionary "american")
	     (flyspell-mode)
	     (set (make-local-variable 'compile-command) "make -C ~/site/schnouki.net")))

;; -----------------------------------------------------------------------------
;; Minor modes
;; -----------------------------------------------------------------------------

;; HideShow minor mode for common major modes
(dolist (hook '(c-mode-common-hook emacs-lisp-mode-hook java-mode-hook lisp-mode-hook
		lua-mode perl-mode-hook python-mode sh-mode-hook))
  (add-hook hook 'hs-minor-mode))

;; smerge-mode, as suggested in the doc
(autoload 'smerge-mode "smerge-mode" nil t)
(setq smerge-command-prefix (kbd "C-c '"))
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)

;; Display the current function name in the mode line
(which-function-mode 1)

;; pretty-lambda
(add-to-list 'pretty-lambda-auto-modes 'python-mode)
(pretty-lambda-for-modes nil)
