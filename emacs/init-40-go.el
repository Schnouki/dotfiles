;;; 40-go --- Go-specific stuff
;;; Commentary:
;;; Code:

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :commands (godoc gofmt gofmt-before-save)
  :bind (:map go-mode-map
              ("C-c C-k" . godoc)
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c C-n" . go-rename)
              ("C-c C-t" . go-add-tags)
              )
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)

  :config
  (defadvice go-root-and-paths (around schnouki/go-root-and-paths)
    (let* ((root-and-paths ad-do-it)
           (root (car root-and-paths))
           (env-paths (cdr root-and-paths))
           (guessed-paths (split-string (go-guess-gopath) path-separator)))
      (setq ad-return-value (cons root (append guessed-paths env-paths)))))
  (ad-activate 'go-root-and-paths)
  (add-hook 'go-mode-hook #'go-set-project))

(use-package company-go
  :ensure t
  :commands company-go
  :init (add-to-list 'company-backends 'company-go))

(use-package go-eldoc
  :ensure t
  :commands go-eldoc-setup
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-guru
  :ensure t
  :commands go-guru-hl-identifier-mode
  :init (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(use-package go-rename
  :ensure t
  :commands go-rename)

(use-package go-add-tags
  :ensure t
  :commands go-add-tags)

;; Patched versions of go-packages-native and go-packages-go-list that strip
;; "vendor" dirs (https://github.com/dominikh/go-mode.el/issues/135)
(defun schnouki/go-packages-strip-vendor (packages)
  (mapcar (lambda (pkg)
	    (if (string-match "/vendor/\\(.*\\)" pkg)
		(match-string 1 pkg)
	      pkg))
	  packages))
(defun schnouki/go-packages-native-without-vendor ()
  "Return a list of all installed Go packages, stripping vendor directories."
  (schnouki/go-packages-strip-vendor (go-packages-native)))
(defun schnouki/go-packages-go-list-without-vendor ()
  "Return a list of all Go packages, using `go list', stripping vendor directories."
  (schnouki/go-packages-strip-vendor (go-packages-go-list)))
(setq go-packages-function 'schnouki/go-packages-go-list-without-vendor)

;;; init-40-go.el ends here
