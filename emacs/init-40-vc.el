;; -----------------------------------------------------------------------------
;; Version control
;; -----------------------------------------------------------------------------

(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-<") 'magit-status)
(setq magit-process-popup-time 5)