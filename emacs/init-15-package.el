;;; 15-package --- Emacs official package manager
;;; Commentary:
;;; Code:

(require 'package)

;; Repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Don't auto-activate packages
(setq package-enable-at-startup nil)
(package-initialize nil)

;; Install use-packages, which will install everything else as needed
(unless (package-installed-p 'use-package)
  (message "Installing use-package…")
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Notify that Emacs is loading…
(use-package alert
  :ensure t
  :commands alert
  :config
  (setq alert-default-style 'libnotify))

(alert (concat "Emacs " emacs-version " is loading…")
       :severity 'low
       :icon "emacs"
       :title "Emacs")

;;; init-15-package.el ends here
