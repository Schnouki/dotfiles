;;; 40-vc --- version control
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :bind (("C-<"   . magit-status)
	 ("C-M-<" . magit-grep))
  :demand t
  :init
  (progn
    (setq magit-process-popup-time -1
          magit-auto-revert-mode-lighter nil
          magit-last-seen-setup-instructions "1.4.0"
          magit-revert-buffers 'silent
          vc-follow-symlinks t)))

(use-package magit-blame
  :ensure magit
  :bind (("C-c C-<" . magit-blame)))

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

;;; init-40-vc.el ends here
