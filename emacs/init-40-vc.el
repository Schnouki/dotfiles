;;; 40-vc --- version control
;;; Commentary:
;;; Code:

(require 'magit)
(global-set-key (kbd "C-<") 'magit-status)
(global-set-key (kbd "C-à") 'magit-status)

(autoload 'magit-blame-mode "magit-blame" nil t)
(global-set-key (kbd "C-c C-<") 'magit-blame-mode)
(global-set-key (kbd "C-c C-à") 'magit-blame-mode)

(setq magit-process-popup-time 5)

(require 'git-gutter-fringe)
(setq git-gutter-fr:side 'right-fringe
      git-gutter:lighter " GG")
(global-git-gutter-mode t)

(global-set-key (kbd "C-! g g") 'git-gutter:toggle)
(global-set-key (kbd "C-! g d") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-! g s") 'git-gutter:stage-hunk)
(global-set-key (kbd "C-! g v") 'git-gutter:revert-hunk)
(global-set-key (kbd "C-! g n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-! g p") 'git-gutter:previous-hunk)

;;; init-40-vc.el ends here
