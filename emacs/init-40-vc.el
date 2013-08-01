;; -----------------------------------------------------------------------------
;; Version control
;; -----------------------------------------------------------------------------

(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-<") 'magit-status)
(global-set-key (kbd "C-à") 'magit-status)

(autoload 'magit-blame-mode "magit" nil t)
(global-set-key (kbd "C-c C-<") 'magit-blame-mode)
(global-set-key (kbd "C-c C-à") 'magit-blame-mode)

(setq magit-process-popup-time 5)
