;;; 30-yasnippet --- yasnippet
;;; Commentary:
;;; Code:

(use-package yasnippet
  :ensure t
  :defer 15
  :delight yas-minor-mode
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)

;; Modes that bind something to "<tab>" instead of "TAB"
;; -- http://blog.iany.me/2012/03/fix-tab-binding-for-yasnippet-and-auto-complete/
(defun schnouki/yas-tab-noconflict ()
  (interactive)
  (let ((command (key-binding [tab])))    ; remember command
    (local-unset-key [tab])               ; unset from (kbd "<tab>")
    (local-set-key (kbd "TAB") command))) ; bind to (kbd "TAB")
(add-hook 'ruby-mode-hook 'schnouki/yas-tab-noconflict)
(add-hook 'markdown-mode-hook 'schnouki/yas-tab-noconflict)

;;; init-30-yasnippet.el ends here
