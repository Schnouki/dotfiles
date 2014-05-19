;;; 15-package --- Emacs official package manager
;;; Commentary:
;;; Code:

(require 'package)

;; Repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Don't auto-activate packages
(setq package-enable-at-startup nil)
(package-initialize)

;; Install use-packages, which will install everything else as needed
(unless (package-installed-p 'use-package)
  (message "Installing use-package…")
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;; init-15-package.el ends here
