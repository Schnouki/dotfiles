;;; 40-vc --- version control
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :bind (("C-<"   . magit-status)
	 ("C-M-<" . vc-git-grep))
  :demand t
  :init
  (setq magit-process-popup-time -1
        magit-auto-revert-mode-lighter nil
        magit-last-seen-setup-instructions "1.4.0"
        magit-revert-buffers 'silent
        magit-push-always-verify nil
        vc-follow-symlinks t))

(use-package magit-blame
  :ensure magit
  :bind (("C-c C-<" . magit-blame)))

(use-package forge
  :ensure t)

;; git-annex
(use-package git-annex
  :ensure t)
(use-package magit-annex
  :ensure t
  :defer t
  :init
  (add-hook 'magit-mode-hook (lambda () (require 'magit-annex))))

;; gitflow
;; (use-package magit-gitflow
;;   :ensure t
;;   :commands turn-on-magit-gitflow
;;   :diminish magit-gitflow-mode
;;   :init
;;   (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

;; gitignore
(use-package gitignore-mode
  :ensure t)

;; TODOs
(use-package magit-todos
  :ensure t
  :defer t
  :config
  (magit-todos-mode t))

;;; init-40-vc.el ends here
