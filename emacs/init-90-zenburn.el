;; -----------------------------------------------------------------------------
;; zenburn color theme
;; -----------------------------------------------------------------------------

(require 'color-theme)

(defun schnouki/use-zenburn ()
  (load "~/.config/emacs/zenburn.el" nil t t)
  (color-theme-zenburn)
  (defvar zenburn-bg-1 "#2f2f2f")
  (defvar zenburn-bg-2 "#1f1f1f")
  (defvar zenburn-bg+4 "#7f7f7f")

  (set-face-background 'hl-line zenburn-bg+1)
  (set-face-attribute 'hl-line nil :underline nil)
  (set-face-background 'show-paren-match-face zenburn-blue-4)
  (set-face-background 'show-paren-mismatch-face zenburn-red-4)
  (set-face-attribute 'show-paren-mismatch-face nil :weight 'bold)
  (set-face-attribute 'show-paren-match-face nil :weight 'normal)

  ;; Tag/face mapping in notmuch
  (setq notmuch-search-line-faces '(("deleted" . '(:background "#6f3f3f"))
				    ("draft"   . '(:slant italic))
				    ("flagged" . '(:background "#5f3f5f"))
				    ("sent"    . '(:weight bold))
				    ("todo"    . '(:background "#3f3f5f"))
				    ("unread"  . '(:background "#3f4f3f"))))

  (eval-after-load 'magit
    '(progn
       (set-face-background 'magit-item-highlight zenburn-bg-1)
       (set-face-foreground 'magit-diff-add zenburn-green)
       (set-face-foreground 'magit-diff-del zenburn-red)
       (set-face-foreground 'magit-log-graph zenburn-bg+4)
       ))

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

  (eval-after-load 'rst
    '(progn
       (set-face-background 'rst-level-1-face "#4f4f4f")
       (set-face-background 'rst-level-2-face "#575757")
       (set-face-background 'rst-level-3-face "#5f5f5f")
       (set-face-background 'rst-level-4-face "#676767")
       (set-face-background 'rst-level-5-face "#6f6f6f")
       (set-face-background 'rst-level-6-face "#777777")))

  (defun schnouki/zenburn-for-auctex ()
    (set-face-foreground 'font-latex-sectioning-5-face zenburn-blue)
    (set-face-foreground 'font-latex-math-face         zenburn-orange)
    (set-face-foreground 'font-latex-verbatim-face     zenburn-orange)
    (set-face-foreground 'font-latex-italic-face       zenburn-green))
  (add-hook 'LaTeX-mode-hook 'schnouki/zenburn-for-auctex)

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

  (defun schnouki/zenburn-for-smerge ()
    (set-face-foreground 'smerge-mine zenburn-cyan)
    (set-face-foreground 'smerge-other zenburn-green+3)
    ;;(set-face-foreground 'smerge-base zenburn-orange)
    (set-face-background 'smerge-markers zenburn-bg+1)
    (set-face-background 'smerge-refined-change zenburn-red-4))
  (add-hook 'smerge-mode-hook 'schnouki/zenburn-for-smerge)

  (defun schnouki/zenburn-for-golbarg ()
    (set-face-background golbarg-header-face zenburn-bg-1))
  (add-hook 'golbarg-mode-hook 'schnouki/zenburn-for-golbarg)
)

(if (file-exists-p "~/.config/emacs/zenburn.el")
    (schnouki/use-zenburn))
