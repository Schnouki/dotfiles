;; -----------------------------------------------------------------------------
;; yasnippet
;; -----------------------------------------------------------------------------

(require 'yasnippet)

(yas-global-mode 1)

;; Snippets dir:
;; - filter our elpy
;; - make sure the local one (~/.emacs.d/snippets) comes first
(setq yas-snippet-dirs
      (cons "~/.emacs.d/snippets"
	    (cl-remove-if (lambda (item)
			    (or
			     (string-equal "~/.emacs.d/snippets" item)
			     (string-match "/elpy-.*/snippets" item)))
			  yas-snippet-dirs)))

;; Modes that bind something to "<tab>" instead of "TAB"
;; -- http://blog.iany.me/2012/03/fix-tab-binding-for-yasnippet-and-auto-complete/
(defun schnouki/yas-tab-noconflict ()
  (interactive)
  (let ((command (key-binding [tab])))    ; remember command
    (local-unset-key [tab])               ; unset from (kbd "<tab>")
    (local-set-key (kbd "TAB") command))) ; bind to (kbd "TAB")
(add-hook 'ruby-mode-hook 'schnouki/yas-tab-noconflict)
(add-hook 'markdown-mode-hook 'schnouki/yas-tab-noconflict)
