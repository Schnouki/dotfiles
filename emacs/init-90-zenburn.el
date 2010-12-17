;; -----------------------------------------------------------------------------
;; zenburn color theme
;; -----------------------------------------------------------------------------

(require 'color-theme)
(require 'zenburn)

(zenburn)

;; Tag/face mapping in notmuch
(setq notmuch-search-line-faces '(("delete"  . '(:background "#3b2b2b"))
				  ("draft"   . '(:slant italic))
				  ("flagged" . '(:background "#4b2f4b"))
				  ("sent"    . '(:weight bold))
				  ("todo"    . '(:background "#2f2f4b"))
				  ("unread"  . '(:background "#2b3b2b"))))
(set-face-foreground 'notmuch-search-date                 zenburn-yellow)
(set-face-foreground 'notmuch-search-count                zenburn-cyan)
(set-face-foreground 'notmuch-search-subject              zenburn-fg)
(set-face-foreground 'notmuch-search-matching-authors     "#ffeece") ;; zenburn-yellow+1
(set-face-foreground 'notmuch-search-non-matching-authors "#b09f6f") ;; zenburn-yellow-4
(set-face-attribute  'notmuch-tag-face                nil :foreground zenburn-green+2 :slant 'italic)

;; Org-mode keywords
(eval-after-load 'org
  '(progn
     (setq org-todo-keyword-faces
	   '(("STARTED"   . "#8cd0d3") ; zenburn-blue
	     ("CANCELED"  . "#7f7f7f") ; zenburn-bg+4

	     ("WIP"       . "#8cd0d3") ; zenburn-blue
	     ("PROPOSÉ"   . "#7f9f7f") ; zenburn-green
	     ("ABANDONNÉ" . "#7f7f7f") ; zenburn-bg+4
	     ("REFUSÉ"    . "#7f7f7f") ; zenburn-bg+4
	     ))))

;; ReST-mode (not really zenburn...)
(eval-after-load 'rst
  '(progn
     (set-face-background 'rst-level-1-face "#4f4f4f")
     (set-face-background 'rst-level-2-face "#575757")
     (set-face-background 'rst-level-3-face "#5f5f5f")
     (set-face-background 'rst-level-4-face "#676767")
     (set-face-background 'rst-level-5-face "#6f6f6f")
     (set-face-background 'rst-level-6-face "#777777")))

;; Ediff (weird, does not work in zenburn.el...)
(defun schnouki/zenburn-for-ediff ()
  ;; Non-active regions
  (set-face-background ediff-even-diff-face-A zenburn-bg+1)
  (set-face-background ediff-even-diff-face-B zenburn-bg+1)
  (set-face-background ediff-even-diff-face-C zenburn-bg+1)

  (set-face-background ediff-odd-diff-face-A zenburn-bg+2)
  (set-face-background ediff-odd-diff-face-B zenburn-bg+2)
  (set-face-background ediff-odd-diff-face-C zenburn-bg+2)

  (set-face-foreground ediff-even-diff-face-A zenburn-fg)
  (set-face-foreground ediff-even-diff-face-B zenburn-fg)
  (set-face-foreground ediff-even-diff-face-C zenburn-fg)
  (set-face-foreground ediff-odd-diff-face-A zenburn-fg)
  (set-face-foreground ediff-odd-diff-face-B zenburn-fg)
  (set-face-foreground ediff-odd-diff-face-C zenburn-fg)

  ;; Active regions
  (set-face-background ediff-current-diff-face-A zenburn-green-1)
  (set-face-foreground ediff-current-diff-face-A zenburn-fg)
  (set-face-background ediff-fine-diff-face-A zenburn-green+2)
  (set-face-foreground ediff-fine-diff-face-A zenburn-bg)

  (set-face-background ediff-current-diff-face-B zenburn-yellow-1)
  (set-face-foreground ediff-current-diff-face-B "black")
  (set-face-background ediff-fine-diff-face-B zenburn-orange)
  (set-face-foreground ediff-fine-diff-face-B zenburn-bg)

  (set-face-background ediff-current-diff-face-C zenburn-red-1)
  (set-face-foreground ediff-current-diff-face-C zenburn-bg-1)
  (set-face-background ediff-fine-diff-face-C zenburn-red+1)
  (set-face-foreground ediff-fine-diff-face-C zenburn-bg)
  )
(add-hook 'ediff-load-hook 'schnouki/zenburn-for-ediff)

(defun schnouki/zenburn-for-golbarg ()
  (set-face-background golbarg-header-face zenburn-bg-1))
(add-hook 'golbarg-mode-hook 'schnouki/zenburn-for-golbarg)

;; Undo-Tree
(eval-after-load 'undo-tree
  '(progn
     (set-face-foreground 'undo-tree-visualizer-default-face zenburn-bg+4)
     (set-face-foreground 'undo-tree-visualizer-current-face zenburn-red)
     (set-face-foreground 'undo-tree-visualizer-active-branch-face zenburn-fg)))

;; OfflineIMAP
(eval-after-load 'offlineimap
  '(progn
     (set-face-foreground 'offlineimap-msg-acct-face             zenburn-magenta)
     (set-face-foreground 'offlineimap-msg-connecting-face       zenburn-bg+4)
     (set-face-foreground 'offlineimap-msg-syncfolders-face      zenburn-blue)
     (set-face-foreground 'offlineimap-msg-syncingfolders-face   zenburn-cyan)
     (set-face-foreground 'offlineimap-msg-skippingfolder-face   zenburn-cyan)
     (set-face-foreground 'offlineimap-msg-loadmessagelist-face  zenburn-green)
     (set-face-foreground 'offlineimap-msg-syncingmessages-face  zenburn-blue)
     (set-face-foreground 'offlineimap-msg-copyingmessage-face   zenburn-orange)
     (set-face-foreground 'offlineimap-msg-deletingmessages-face zenburn-red)
     (set-face-foreground 'offlineimap-msg-deletingmessage-face  zenburn-red)
     (set-face-foreground 'offlineimap-msg-addingflags-face      zenburn-yellow)
     (set-face-foreground 'offlineimap-msg-deletingflags-face    zenburn-magenta)
     (set-face-foreground 'offlineimap-error-face                zenburn-red)))
