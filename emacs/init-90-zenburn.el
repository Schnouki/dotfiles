;; -----------------------------------------------------------------------------
;; zenburn color theme
;; -----------------------------------------------------------------------------

(require 'color-theme)
(require 'zenburn)

(zenburn)

;; Tag/face mapping in notmuch
(setq notmuch-search-line-faces '(("deleted" . '(:background "#6f3f3f"))
				  ("draft"   . '(:slant italic))
				  ("flagged" . '(:background "#5f3f5f"))
				  ("sent"    . '(:weight bold))
				  ("todo"    . '(:background "#3f3f5f"))
				  ("unread"  . '(:background "#3f4f3f"))))

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
