;;; 25-helm -- Everything about helm
;;; Commentary:
;;;
;;; Relevant docs:
;;; - https://tuhdo.github.io/helm-intro.html
;;;
;;; Code:

(use-package helm
  :ensure t
  :diminish helm-mode
  :demand t
  :bind (("M-x"       . helm-M-x)
	 ("C-² C-SPC" . helm-all-mark-rings)
	 ("C-x C-f"   . helm-find-files)
	 ("C-² C-g"   . helm-google-suggest)
	 ("C-x b"     . helm-mini)
	 ("C-² o"     . helm-occur)
	 ("C-M-y"     . helm-show-kill-ring))
  :config
  (progn
    (require 'helm-config)

    (global-set-key (kbd "C-²") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    (when (executable-find "ack")
      (setq helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
            helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f"))

    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	  helm-ff-skip-boring-files             t
          helm-ff-file-name-history-use-recentf t
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	  helm-quick-update                     t
          ;; fuzzy matching
          helm-M-x-fuzzy-match        t
          helm-apropos-fuzzy-match    t
          helm-buffers-fuzzy-matching t
          helm-imenu-fuzzy-match      t
          helm-locate-fuzzy-match     t
          helm-recentf-fuzzy-match    t
          helm-semantic-fuzzy-match   t)
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

    (helm-mode)))

(use-package helm-c-yasnippet
  :ensure t
  :bind ("C-² y" . helm-yas-complete)
  :config
  (setq helm-yas-space-match-any-greedy t
        helm-yas-display-key-on-candiadte t))

(use-package helm-company
  :ensure t
  :commands helm-company
  :init
  (eval-after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-:") 'helm-company)
       (define-key company-active-map (kbd "C-:") 'helm-company))))

(use-package helm-descbinds
  :ensure t
  :config (helm-descbinds-mode))

(use-package helm-flycheck
  :ensure t
  :commands helm-flycheck
  :init
  (eval-after-load 'flycheck
    '(bind-key "h" 'helm-flycheck flycheck-command-map)))

(use-package helm-git-grep
  :ensure t
  :bind (("C-² g"   . helm-git-grep)
         ("C-² M-g" . helm-git-grep-at-point)))

(use-package helm-go-package
  :ensure t
  :commands helm-go-package
  :init
  (eval-after-load 'go-mode
    '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map)))

(use-package helm-ls-git
  :ensure t
  :config
  (progn
    ;;; TODO configure
  ))

(use-package helm-swoop
  :ensure t
  :bind (("C-² C-s" . helm-swoop)
         ("C-² M-s" . helm-multi-swoop-current-mode))
  :config
  (progn
    (setq helm-swoop-use-line-number-face t
          helm-swoop-speed-or-color t)
    (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)))


;;; init-80-helm.el ends here
