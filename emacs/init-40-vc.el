;;; 40-vc --- version control
;;; Commentary:
;;; Code:

(use-package magit
  :ensure magit
  :bind (("C-<" . magit-status)
	 ("C-M-<" . magit-grep))
  :init
  (progn
    (setq magit-process-popup-time 5
	  magit-auto-revert-mode-lighter nil)))

(use-package magit-blame
  :ensure magit
  :bind (("C-c C-<" . magit-blame-mode)))

;; git-annex
(use-package git-annex
  :ensure git-annex)
(use-package magit-annex
  :ensure magit-annex
  :defer t
  :init
  (add-hook 'magit-mode-hook (lambda () (require 'magit-annex))))

;; gitflow
(use-package magit-gitflow
  :ensure magit-gitflow
  :commands turn-on-magit-gitflow
  :init
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

;; gitignore
(use-package gitignore-mode
  :ensure gitignore-mode)

;;; init-40-vc.el ends here
