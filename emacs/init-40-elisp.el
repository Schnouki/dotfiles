;;; 40-elisp --- Elisp-specific stuff
;;; Commentary:
;;; Code:

(use-package package-lint
  :ensure t)
(use-package flycheck-package
  :ensure t
  :commands flycheck-package-setup
  :init
  (eval-after-load 'flycheck
    '(flycheck-package-setup)))

(use-package faceup
  :ensure t)

;;; init-40-elisp.el ends here
