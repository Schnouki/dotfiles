;;; 50-projectile --- Projectile config
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching nil
        projectile-sort-order 'recently-active)

  ;; Mode line
  (defun schnouki/projectile-mode-line ()
    (if (file-remote-p default-directory)
        " Pj"
      (format " Pj[%s]" (projectile-project-name))))
  (setq projectile-mode-line-function 'schnouki/projectile-mode-line)

  ;; Ignore suffixes
  (seq-each (lambda (suf) (add-to-list 'projectile-globally-ignored-file-suffixes suf))
            '(".pyc" ".o" ".so" "~" "#" ".min.js"))

  ;; Keybindings
  (bind-key "s-!" 'projectile-command-map projectile-mode-map)
  (bind-key "s-/" 'projectile-command-map projectile-mode-map)
  (bind-key "s s" 'deadgrep 'projectile-command-map)
  (bind-key "b" 'projectile-ibuffer schnouki-prefix-map)

  ;; Set virtualenv packages as Projectile projects -- based on
  ;; https://github.com/bbatsov/projectile/issues/364#issuecomment-61296248 and
  ;; https://emacs.stackexchange.com/a/2924
  (defvar schnouki/projectile-project-root-regexps ()
    "List of regexps to match against when Projectile is searching for project root directories.")

  (add-to-list 'schnouki/projectile-project-root-regexps
               "~/\.virtualenvs/[^/]+/\\(local/\\)?lib/python[^/]*/site-packages/[^/]+/?$")
  (add-to-list 'schnouki/projectile-project-root-regexps
               "~/\.local/share/virtualenvs/[^/]+/\\(local/\\)?lib/python[^/]*/site-packages/[^/]+/?$")

  (defun schnouki/projectile-root-regexp (dir)
    (projectile-locate-dominating-file
     dir
     (lambda (dir)
       (seq-find (lambda (it)
                   (if (and
                        (s-equals? (file-remote-p it) (file-remote-p dir))
                        (string-match-p (expand-file-name it) (expand-file-name dir)))
                       dir))
                 schnouki/projectile-project-root-regexps))))

  (add-to-list 'projectile-project-root-functions #'schnouki/projectile-root-regexp t)

  (defun schnouki/projectile-magit ()
    "Open a magit-status buffer in a known project."
    (interactive)
    (let ((projects (projectile-relevant-known-projects)))
      (if projects
          (projectile-completing-read
           "Open magit in project: " projects
           :action (lambda (project)
                     (magit-status project)))
        (user-error "There are no known projects"))))
  (bind-key "\\" 'schnouki/projectile-magit projectile-command-map)
  (bind-key "<" 'schnouki/projectile-magit projectile-command-map)

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

(use-package deadgrep
  :ensure t
  :commands deadgrep
  :bind (:map deadgrep-mode-map
              ("C-x C-q" . deadgrep-edit-mode)
              :map deadgrep-edit-mode-map
              ("C-c C-c" . deadgrep-mode))
  :config
  (defun schnouki/deadgrep--guess-type ()
    (let* ((deadgrep-types (deadgrep--type-list))
           (ext-to-type (seq-mapcat (lambda (it)
                                      (let ((type-name (car it))
                                            (exts (cadr it)))
                                        (mapcar (lambda (typ) (cons
                                                               (s-chop-prefix "*." typ)
                                                               type-name))
                                                exts)))
                                    deadgrep-types))
           (file-ext (s-chop-prefix "." (url-file-extension (buffer-name)))))
      (cdr (assoc file-ext ext-to-type))))
  (defun schnouki/deadgrep--auto-guess-type (orig-fun &rest args)
    (let* ((guessed-type (schnouki/deadgrep--guess-type))
           (new-type (if guessed-type
                         (cons 'type guessed-type)
                       deadgrep--file-type)))
      (let ((deadgrep--file-type new-type))
        (apply orig-fun args))))
  (advice-add 'deadgrep :around #'schnouki/deadgrep--auto-guess-type)

  (defun schnouki/deadgrep--avoid-small-searches (args)
    (when (< (length (car args)) 2)
      (error "Search term too short: %s" (car args)))
    args)
  (advice-add 'deadgrep--start :filter-args #'schnouki/deadgrep--avoid-small-searches))

(use-package defproject
  :ensure t
  :commands defproject)

;;; init-50-projectile.el ends here
