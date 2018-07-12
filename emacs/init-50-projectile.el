;;; 50-projectile --- Projectile config
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "s-!")
        projectile-mode-line '(:eval
                              (if
                                  (file-remote-p default-directory)
                                  " Projectile"
                                (format " Pj[%s]"
                                        (projectile-project-name)))))
  :config
  (--each '(".pyc" ".o" ".so" "~" "#" ".min.js")
    (add-to-list 'projectile-globally-ignored-file-suffixes it))
  (bind-key "s s" 'projectile-ripgrep 'projectile-command-map)

  ;; Set virtualenv packages as Projectile projects -- based on
  ;; https://github.com/bbatsov/projectile/issues/364#issuecomment-61296248 and
  ;; https://emacs.stackexchange.com/a/2924
  (defvar schnouki/projectile-project-root-regexps ()
    "List of regexps to match against when Projectile is searching for project root directories.")

  (add-to-list 'schnouki/projectile-project-root-regexps
               "~/\.virtualenvs/[^/]+/\\(local/\\)?lib/python[^/]*/site-packages/[^/]+/?$")

  (defun schnouki/projectile-root-regexp (dir &optional list)
    (projectile-locate-dominating-file
     dir
     (lambda (dir)
       (--first
        (if (and
             (s-equals? (file-remote-p it) (file-remote-p dir))
             (string-match-p (expand-file-name it) (expand-file-name dir)))
            dir)
        (or list schnouki/projectile-project-root-regexps (list))))))

  (add-to-list 'projectile-project-root-files-functions #'schnouki/projectile-root-regexp t)

  (projectile-mode 1))

(use-package ibuffer-projectile
  :ensure t
  :config
  (defun schnouki/enable-ibuffer-projectile ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  (add-hook 'ibuffer-hook #'schnouki/enable-ibuffer-projectile))

(use-package ripgrep
  :ensure t
  :commands ripgrep-regexp)


;;; init-50-projectile.el ends here
