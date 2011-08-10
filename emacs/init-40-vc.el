;; -----------------------------------------------------------------------------
;; Version control
;; -----------------------------------------------------------------------------

(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-<") 'magit-status)
(global-set-key (kbd "C-à") 'magit-status)
(setq magit-process-popup-time 5)