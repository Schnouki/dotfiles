;;; 15-package --- Emacs official package manager
;;; Commentary:
;;; Code:

(require 'package)

;; Repositories
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Don't auto-activate packages
(setq package-enable-at-startup nil)
(package-initialize nil)

;; Install use-packages, which will install everything else as needed
(unless (package-installed-p 'use-package)
  (message "Installing use-package…")
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(use-package use-package
  :ensure t)
(use-package diminish
  :ensure t)

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

;;; init-15-package.el ends here
