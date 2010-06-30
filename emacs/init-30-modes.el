;; -----------------------------------------------------------------------------
;; Major modes
;; -----------------------------------------------------------------------------

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(autoload 'lua-mode "lua-mode" "Lua mode." t)
(setq auto-mode-alist (append '(("\\.lua$" . lua-mode)) auto-mode-alist))

(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("\\.py$" . python-mode)) auto-mode-alist))

(autoload 'php-mode "php-mode.el" "Php mode." t)
(setq auto-mode-alist (append '(("\\.php[345]?$" . php-mode)) auto-mode-alist))

(autoload 'yaml-mode "yaml-mode.el" "Yaml mode." t)
(setq auto-mode-alist (append '(("\\.ya?ml$" . yaml-mode)) auto-mode-alist))

(autoload 'markdown-mode "markdown-mode.el" "Markdown mode." t)
(setq auto-mode-alist (append '(("\\.md$" . markdown-mode) ("\\.markdown$" . markdown-mode)) auto-mode-alist))
(add-hook 'markdown-mode-hook '(lambda () (setq markdown-command "~/.config/emacs/markdown.sh")))

(autoload 'jinja-mode "jinja.el" "Jinja mode." t)

(autoload 'cuda-mode "cuda-mode.el" "Cuda mode." t)
(setq auto-mode-alist (append '(("\\.cu$" . cuda-mode)) auto-mode-alist))

(autoload 'cmake-mode "cmake-mode.el" "CMake mode." t)
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

;; pkgbuild-mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))

;; gnuplot-mode
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;; Golbarg, the most awesome blog engine ever
(require 'golbarg)
(setq golbarg-posts-dir "~/site/www/posts")
(setq golbarg-drafts-dir "~/site/www/drafts")
(global-set-key (kbd "C-! g") 'golbarg-new-draft)
(global-set-key (kbd "C-! M-g") '(lambda () (interactive) (find-file golbarg-drafts-dir)))
(setq auto-mode-alist (append `((,(concat "^" (expand-file-name "~/site/www/") "/.+") . golbarg-mode)) auto-mode-alist))
(add-hook 'golbarg-mode-hook 
	  '(lambda ()
	     (turn-on-auto-fill)
	     (ispell-change-dictionary "american")
	     (flyspell-mode)
	     (setq compile-command "make -C ~/site/www")))

;; smerge-mode, as suggested in the doc
(autoload 'smerge-mode "smerge-mode" nil t)
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)
