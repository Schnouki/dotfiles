;;; 15-package --- Emacs official package manager  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)

;; Repositories
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Don't auto-activate packages
(setq package-enable-at-startup nil)
(package-initialize nil)

;; Review all packages before install/update
(setq package-review-policy t
      package-review-diff-command '("git" "diff" "--no-index" "--color=never" "--diff-filter=d"))

;; Install use-packages, which will install everything else as needed
(unless (package-installed-p 'use-package)
  (message "Installing use-package…")
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(use-package use-package
  :ensure t)
(use-package delight
  :ensure t)

;; Make sure packaqge-selected-packages is up-to-date
(defun schnouki/use-package-ensure-selected-advice (name &rest rest)
  (when (and
         (not (package--user-selected-p name))
         (package-installed-p name))
    (package--save-selected-packages
     (cons name package-selected-packages))))
(advice-add #'use-package-handler/:ensure :after #'schnouki/use-package-ensure-selected-advice)

;; Selected packages that don't use use-package
(defun schnouki/check-useless-packages ()
  (let* ((used-packages (mapcar #'intern (split-string (shell-command-to-string "rg -I '^\\s*\\(use-package\\s+' ~/.config/emacs | awk '{print $2}' | sort -u"))))
         (missing (cl-set-difference used-packages
                                     package-selected-packages))
         (extra (cl-set-difference package-selected-packages
                                   used-packages)))
    (message "Missing packages: %s\nUnneeded packages: %s"
             (mapconcat #'symbol-name missing ", ")
             (mapconcat #'symbol-name extra ", "))))
;; (schnouki/check-useless-packages)

;; Update selected package to only include packages that are actually installed...
(defun schnouki/update-selected-packages ()
  (let* ((new-selected (cl-remove-if-not #'package-installed-p package-selected-packages))
         (removed (cl-set-difference package-selected-packages new-selected)))
    (package--save-selected-packages new-selected)
    (message "Removed from selected packages: %s"
             (mapconcat #'symbol-name removed ", "))))
;; (schnouki/update-selected-packages)

;; Notify that Emacs is loading…
(use-package alert
  :ensure t
  :commands alert
  :custom
  (alert-default-style 'libnotify))

(alert (concat "Emacs " emacs-version " is loading…")
       :severity 'low
       :icon "emacs"
       :title "Emacs"
       :id 'emacs-startup)

(defvar schnouki/init-emacs--ready-alerted nil)
(defun schnouki/init-emacs--alert-ready (&rest args)
  (unless schnouki/init-emacs--ready-alerted
    (alert (concat "Emacs " emacs-version" loaded!")
           :severity 'low
           :icon "emacs"
           :title "Emacs"
           :id 'emacs-startup)
    (setq schnouki/init-emacs--ready-alerted t)))
(advice-add 'server-start :after #'schnouki/init-emacs--alert-ready)

(defun schnouki/custom-reset-variable (variable)
  "Reset VARIABLE to its standard value."
  (set variable (custom--standard-value variable)))

(defun schnouki/custom-reset-group (group)
  "Reset all custom variables in GROUP to their standard value."
  (dolist (member (get 'apheleia 'custom-group))
    (let ((var (car member)))
      (when (custom-variable-p var)
        (set var (custom--standard-value var))))))

;;; init-15-package.el ends here
