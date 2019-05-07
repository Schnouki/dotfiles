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
  (add-to-list 'go-guess-gopath-functions #'schnouki/go-hellogopher-gopath)

  (defun schnouki/go--better-guess-gopath (orig-fun &rest args)
    (let* ((root-and-paths (apply orig-fun args))
	   (root (car root-and-paths))
	   (env-paths (cdr root-and-paths))
	   (guessed-paths (split-string (go-guess-gopath) path-separator)))
      (cons root (append guessed-paths env-paths))))
  (advice-add 'go-root-and-paths :around #'schnouki/go--better-guess-gopath)

  (add-hook 'go-mode-hook #'go-set-project))

(defun schnouki/go-hellogopher-gopath ()
  (let ((d (locate-dominating-file buffer-file-name ".GOPATH")))
    (if d
        (list (concat d
                      (file-name-as-directory ".GOPATH"))))))

;; (use-package company-go
;;   :ensure t
;;   :commands company-go
;;   :init (add-to-list 'company-backends 'company-go))

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

(use-package godoctor
  :ensure t
  :bind (:map go-mode-map
              ("C-c d d" . godoctor-godoc)
              ("C-c d e" . godoctor-extract)
              ("C-c d r" . godoctor-rename)
              ("C-c d t" . godoctor-toggle)))

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
