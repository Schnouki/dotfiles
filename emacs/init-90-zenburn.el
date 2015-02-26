;;; 90-zenburn --- Zenburn color theme
;;; Commentary:
;;; Code:

(use-package color-theme
  :ensure color-theme)
(use-package zenburn-theme
  :ensure zenburn-theme
  :init
  (progn
    (load-theme 'zenburn)

    (zenburn-with-color-variables

      ;; Changes to "global" faces
      (set-face-background 'hl-line zenburn-bg-05)
      (set-face-background 'schnouki/notmuch-hl-line zenburn-bg-05)

      ;; notmuch
      (defun schnouki/get-icon (name)
	(with-temp-buffer
	  (insert-file-contents (concat "/usr/share/icons/Numix/16x16/" name ".svg"))
	  (buffer-string)))
      (setq notmuch-search-line-faces `(("deleted" . (:strike-through t))
					("draft"   . (:background ,zenburn-bg-1))
					("sent"    . (:slant italic))
					("unread"  . (:background ,zenburn-bg+1)))
	    notmuch-tag-formats `(("attachment" (notmuch-tag-format-image-data tag (schnouki/get-icon "mimetypes/archive")))
				  ("encrypted"  (notmuch-tag-format-image-data tag (schnouki/get-icon "status/keys")))
				  ("flagged"    (propertize tag 'face '(:foreground ,zenburn-blue))
				                (notmuch-tag-format-image-data tag (schnouki/get-icon "emblems/emblem-favorite")))
				  ("forwarded"  (propertize tag 'face '(:weight bold :foreground ,zenburn-blue-4)))
				  ("inbox"      (propertize tag 'face '(:weight bold)))
				  ("replied"    (propertize tag 'face '(:weight bold :foreground ,zenburn-blue-2)))
				  ("sent"       (propertize tag 'face '(:weight bold :foreground ,zenburn-cyan)))
				  ("todo"       (propertize tag 'face '(:weight bold :foreground ,zenburn-orange)))
				  ("unread"     (propertize tag 'face '(:foreground ,zenburn-red)))))

      (eval-after-load 'notmuch
	`(progn
	   (set-face-foreground 'notmuch-search-date                 ,zenburn-yellow)
	   (set-face-foreground 'notmuch-search-count                ,zenburn-cyan)
	   (set-face-foreground 'notmuch-search-subject              ,zenburn-fg)
	   (set-face-foreground 'notmuch-search-matching-authors     "#ffeece") ;; zenburn-yellow+1
	   (set-face-foreground 'notmuch-search-non-matching-authors "#b09f6f") ;; zenburn-yellow-4
	   (set-face-attribute  'notmuch-tag-face                nil :foreground ,zenburn-green+2 :slant 'italic)

	   (dolist (face '(notmuch-crypto-signature-good notmuch-crypto-signature-good-key
							 notmuch-crypto-signature-unknown notmuch-crypto-signature-bad
							 notmuch-crypto-decryption notmuch-crypto-part-header))
	     (set-face-attribute face nil :bold t :background ,zenburn-bg-1))

	   (set-face-foreground 'notmuch-crypto-signature-good     ,zenburn-green+3)
	   (set-face-foreground 'notmuch-crypto-signature-good-key ,zenburn-cyan)
	   (set-face-foreground 'notmuch-crypto-signature-bad      ,zenburn-red+1)
	   (set-face-foreground 'notmuch-crypto-signature-unknown  "#bf8f6f") ;; zenburn-orange-2
	   (set-face-foreground 'notmuch-crypto-decryption         ,zenburn-magenta)
	   (set-face-foreground 'notmuch-crypto-part-header        ,zenburn-blue-2)))

      ;; Org-mode keywords
      (eval-after-load 'org
	`(progn
	   (setq org-todo-keyword-faces
		 '(("STARTED"   . "#8cd0d3") ; zenburn-blue
		   ("WIP"       . "#8cd0d3") ; zenburn-blue

		   ("PROPOSÉ"   . "#7f9f7f") ; zenburn-green
		   ("READY"     . "#7f9f7f") ; zenburn-green

		   ("ABANDONNÉ" . "#7f7f7f") ; zenburn-bg+4
		   ("CANCELED"  . "#7f7f7f") ; zenburn-bg+4
		   ("DÉLÉGUÉ"   . "#7f7f7f") ; zenburn-bg+4
		   ("N/A"       . "#7f7f7f") ; zenburn-bg+4
		   ("REFUSÉ"    . "#7f7f7f") ; zenburn-bg+4
		   )

		 org-priority-faces
		 '((?A . (:foreground ,zenburn-yellow   :weight bold)) ; zenburn-yellow
		   (?B . (:foreground ,zenburn-yellow-1 :weight bold)) ; zenburn-yellow-1
		   (?C . (:foreground ,zenburn-yellow-2))              ; zenburn-yellow-2
		   (?D . (:foreground "#c0af7f"))                      ; zenburn-yellow-3
		   (?E . (:foreground "#b09f6f"))                      ; zenburn-yellow-4
		   ))))

      ;; diff and magit-diff
      (eval-after-load 'diff-mode
	'(progn
	   (set-face-foreground 'diff-context "#bcbcac"))) ;; zenburn-fg-2

      ;; smerge minor mode
      (eval-after-load 'smerge
	`(progn
	   (set-face-background 'smerge-refined-change ,zenburn-bg+2)))

      ;; OfflineIMAP
      (eval-after-load 'offlineimap
	`(progn
	   (set-face-foreground 'offlineimap-msg-acct-face             ,zenburn-magenta)
	   (set-face-foreground 'offlineimap-msg-connecting-face       "#7f7f7f") ;; zenburn-bg+4
	   (set-face-foreground 'offlineimap-msg-syncfolders-face      ,zenburn-blue)
	   (set-face-foreground 'offlineimap-msg-syncingfolders-face   ,zenburn-cyan)
	   (set-face-foreground 'offlineimap-msg-skippingfolder-face   ,zenburn-cyan)
	   (set-face-foreground 'offlineimap-msg-loadmessagelist-face  ,zenburn-green)
	   (set-face-foreground 'offlineimap-msg-syncingmessages-face  ,zenburn-blue)
	   (set-face-foreground 'offlineimap-msg-copyingmessage-face   ,zenburn-orange)
	   (set-face-foreground 'offlineimap-msg-deletingmessages-face ,zenburn-red)
	   (set-face-foreground 'offlineimap-msg-deletingmessage-face  ,zenburn-red)
	   (set-face-foreground 'offlineimap-msg-addingflags-face      ,zenburn-yellow)
	   (set-face-foreground 'offlineimap-msg-deletingflags-face    ,zenburn-magenta)
	   (set-face-foreground 'offlineimap-error-face                ,zenburn-red)))

      ;; show-paren-mode
      (eval-after-load 'paren
	`(progn
	   (set-face-attribute 'show-paren-match    nil :weight 'bold :foreground nil :background ,zenburn-bg-1)
	   (set-face-attribute 'show-paren-mismatch nil :weight 'bold :foreground ,zenburn-red :background "#3c1313")))

      ;; folding
      (eval-after-load 'folding
	`(progn
	   (defface folding-header-line '((((class color) (min-colors 89))
					   (:foreground ,zenburn-green+4
							:background ,zenburn-bg-1
							:box (:line-width -1 :style released-button))))
	     "Face for folding headers")
	   (defvar folding-header-line-face 'folding-header-line)
	   (setq-default folding-font-lock-begin-mark 'folding-header-line-face
			 folding-font-lock-end-mark   'folding-header-line-face)))

      ;; hideshowvis
      (eval-after-load 'hideshowvis
	`(progn
	   (set-face-attribute 'hideshowvis-hidable-face nil :foreground ,zenburn-green+3 :box nil)
	   (set-face-attribute 'hs-fringe-face           nil :foreground ,zenburn-green+3 :box nil)
	   (set-face-attribute 'hs-face                  nil :foreground ,zenburn-green+3 :background ,zenburn-bg-1 :box (list :style 'pressed-button))))

      ;; auto-dim-other-bufers
      (eval-after-load 'auto-dim-other-buffers
	`(progn
	   (set-face-background 'auto-dim-other-buffers-face ,zenburn-bg-05)))

      ;; git-annex
      (eval-after-load 'git-annex
	`(progn
	   (set-face-foreground 'git-annex-dired-annexed-available ,zenburn-green+2)
	   (set-face-foreground 'git-annex-dired-annexed-unavailable ,zenburn-red))))))

;;; init-90-zenburn.el ends here
