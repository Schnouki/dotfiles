;; -----------------------------------------------------------------------------
;; Major modes
;; -----------------------------------------------------------------------------

;; HideShow minor mode for common major modes
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'lua-mode             'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

;; Prepare various major modes
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(autoload 'lua-mode "lua-mode" "Lua mode." t)
(setq auto-mode-alist (append '(("\\.lua$" . lua-mode)) auto-mode-alist))
(folding-add-to-marks-list 'lua-mode "-- {{{" "-- }}}" nil t)

(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("\\.py$" . python-mode)) auto-mode-alist))
(add-hook 'python-mode 'hs-minor-mode)

(autoload 'php-mode "php-mode.el" "Php mode." t)
(setq auto-mode-alist (append '(("\\.php[345]?$" . php-mode)) auto-mode-alist))

(autoload 'yaml-mode "yaml-mode.el" "Yaml mode." t)
(setq auto-mode-alist (append '(("\\.ya?ml$" . yaml-mode)) auto-mode-alist))

(autoload 'markdown-mode "markdown-mode.el" "Markdown mode." t)
(setq auto-mode-alist (append '(("\\.md$" . markdown-mode) ("\\.markdown$" . markdown-mode)) auto-mode-alist))
(add-hook 'markdown-mode-hook '(lambda () (setq markdown-command "~/.config/emacs/markdown.sh")))

(autoload 'adoc-mode "adoc-mode.el" "AsciiDoc mode." t)
(setq auto-mode-alist (append '(("\\.adoc?$" . adoc-mode)) auto-mode-alist))

(autoload 'jinja-mode "jinja.el" "Jinja mode." t)

(autoload 'cuda-mode "cuda-mode.el" "Cuda mode." t)
(setq auto-mode-alist (append '(("\\.cu$" . cuda-mode)) auto-mode-alist))
(folding-add-to-marks-list 'cuda-mode "// {{{" "// }}}" nil t)

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
(setq golbarg-posts-dir "~/site/schnouki.net/posts")
(setq golbarg-drafts-dir "~/site/schnouki.net/drafts")
(global-set-key (kbd "C-! g") 'golbarg-new-draft)
(global-set-key (kbd "C-ç g") 'golbarg-new-draft)
(global-set-key (kbd "C-! M-g") '(lambda () (interactive) (find-file golbarg-drafts-dir)))
(global-set-key (kbd "C-ç M-g") '(lambda () (interactive) (find-file golbarg-drafts-dir)))
(setq auto-mode-alist (append `((,(concat "^" (expand-file-name "~/site/schnouki.net/") "/.+") . golbarg-mode)) auto-mode-alist))
(add-hook 'golbarg-mode-hook 
	  '(lambda ()
	     (turn-on-auto-fill)
	     (ispell-change-dictionary "american")
	     (flyspell-mode)
	     (setq compile-command "make -C ~/site/schnouki.net")))

;; smerge-mode, as suggested in the doc
(autoload 'smerge-mode "smerge-mode" nil t)
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)

;; Display the current function name in the mode line
(which-function-mode 1)

;; doxymacs
(defun doxymacs-mode-and-fontify ()
  (doxymacs-mode t)
  (doxymacs-font-lock))
(add-hook 'c-mode-common-hook 'doxymacs-mode-and-fontify)
