;; -----------------------------------------------------------------------------
;; zenburn color theme
;; -----------------------------------------------------------------------------

(load-theme 'zenburn)

(setq zenburn-fg       "#dcdccc"
      zenburn-fg-1     "#656555"
      zenburn-bg-1     "#2b2b2b"
      zenburn-bg-05    "#383838"
      zenburn-bg       "#3f3f3f"
      zenburn-bg+1     "#4f4f4f"
      zenburn-bg+2     "#5f5f5f"
      zenburn-red+1    "#dca3a3"
      zenburn-red      "#cc9393"
      zenburn-red-1    "#bc8383"
      zenburn-red-2    "#ac7373"
      zenburn-red-3    "#9c6363"
      zenburn-red-4    "#8c5353"
      zenburn-orange   "#dfaf8f"
      zenburn-yellow   "#f0dfaf"
      zenburn-yellow-1 "#e0cf9f"
      zenburn-yellow-2 "#d0bf8f"
      zenburn-green-1  "#5f7f5f"
      zenburn-green    "#7f9f7f"
      zenburn-green+1  "#8fb28f"
      zenburn-green+2  "#9fc59f"
      zenburn-green+3  "#afd8af"
      zenburn-green+4  "#bfebbf"
      zenburn-cyan     "#93e0e3"
      zenburn-blue+1   "#94bff3"
      zenburn-blue     "#8cd0d3"
      zenburn-blue-1   "#7cb8bb"
      zenburn-blue-2   "#6ca0a3"
      zenburn-blue-3   "#5c888b"
      zenburn-blue-4   "#4c7073"
      zenburn-blue-5   "#366060"
      zenburn-magenta  "#dc8cc3")

;; Changes to "global" faces
(set-face-background 'highlight zenburn-bg+1)

;; notmuch
(setq notmuch-search-line-faces '(("deleted" . '(:strike-through t))
				  ("draft"   . '(:slant italic :background "#2b2b2b"))
				  ("flagged" . '(:background "#4b2f4b"))
				  ("sent"    . '(:slant italic))
				  ("todo"    . '(:weight bold))
				  ("unread"  . '(:background "#2b3b2b"))))
(eval-after-load 'notmuch
  '(progn
     (set-face-foreground 'notmuch-search-date                 zenburn-yellow)
     (set-face-foreground 'notmuch-search-count                zenburn-cyan)
     (set-face-foreground 'notmuch-search-subject              zenburn-fg)
     (set-face-foreground 'notmuch-search-matching-authors     "#ffeece") ;; zenburn-yellow+1
     (set-face-foreground 'notmuch-search-non-matching-authors "#b09f6f") ;; zenburn-yellow-4
     (set-face-attribute  'notmuch-tag-face                nil :foreground zenburn-green+2 :slant 'italic)

     (mapcar '(lambda (f) (set-face-attribute f nil :bold t :background zenburn-bg-1))
	     '(notmuch-crypto-signature-good notmuch-crypto-signature-good-key
	       notmuch-crypto-signature-unknown notmuch-crypto-signature-bad
	       notmuch-crypto-decryption notmuch-crypto-part-header))

     (set-face-foreground 'notmuch-crypto-signature-good     zenburn-green+3)
     (set-face-foreground 'notmuch-crypto-signature-good-key zenburn-cyan)
     (set-face-foreground 'notmuch-crypto-signature-bad      zenburn-red+1)
     (set-face-foreground 'notmuch-crypto-signature-unknown  "#bf8f6f") ;; zenburn-orange-2
     (set-face-foreground 'notmuch-crypto-decryption         zenburn-magenta)
     (set-face-foreground 'notmuch-crypto-part-header        zenburn-blue-2)))

;; Org-mode keywords
(eval-after-load 'org
  '(progn
     (setq org-todo-keyword-faces
	   '(("STARTED"   . "#8cd0d3") ; zenburn-blue
	     ("WIP"       . "#8cd0d3") ; zenburn-blue

	     ("PROPOSÉ"   . "#7f9f7f") ; zenburn-green
	     ("READY"     . "#7f9f7f") ; zenburn-green

	     ("ABANDONNÉ" . "#7f7f7f") ; zenburn-bg+4
	     ("CANCELED"  . "#7f7f7f") ; zenburn-bg+4
	     ("N/A"       . "#7f7f7f") ; zenburn-bg+4
	     ("REFUSÉ"    . "#7f7f7f") ; zenburn-bg+4
	     )

	   org-priority-faces
	   '((?A . (:foreground "#f0dfaf" :weight bold)) ; zenburn-yellow
	     (?B . (:foreground "#e0cf9f" :weight bold)) ; zenburn-yellow-1
	     (?C . (:foreground "#d0bf8f"))              ; zenburn-yellow-2
	     (?D . (:foreground "#c0af7f"))              ; zenburn-yellow-3
	     (?E . (:foreground "#b09f6f"))              ; zenburn-yellow-4
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

;; diff and magit-diff
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-context "#bcbcac"))) ;; zenburn-fg-2

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
     (set-face-foreground 'undo-tree-visualizer-default-face "#7f7f7f") ; ;zenburn-bg+4
     (set-face-foreground 'undo-tree-visualizer-current-face zenburn-red)
     (set-face-foreground 'undo-tree-visualizer-active-branch-face zenburn-fg)))

;; OfflineIMAP
(eval-after-load 'offlineimap
  '(progn
     (set-face-foreground 'offlineimap-msg-acct-face             zenburn-magenta)
     (set-face-foreground 'offlineimap-msg-connecting-face       "#7f7f7f") ;; zenburn-bg+4
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

;; show-paren-mode
(eval-after-load 'paren
  '(progn
     (set-face-attribute 'show-paren-match    nil :weight 'bold :foreground nil :background zenburn-bg-1)
     (set-face-attribute 'show-paren-mismatch nil :weight 'bold :foreground zenburn-red :background "#3c1313")))

;; message-mode
(eval-after-load 'message
  '(progn
     (let ((faces `((message-cited-text        ,zenburn-green   nil)
		    (message-header-name       ,zenburn-green+1 nil)
		    (message-header-other      ,zenburn-green   nil)
		    (message-header-to         ,zenburn-yellow  t)
		    (message-header-cc         ,zenburn-yellow  t)
		    (message-header-newsgroups ,zenburn-yellow  t)
		    (message-header-subject    ,zenburn-orange  t)
		    (message-header-xheader    ,zenburn-green   nil)
		    (message-mml               ,zenburn-yellow  t))))
       (dolist (item faces)
	 (set-face-foreground (car item) (cadr item))
	 (set-face-bold-p (car item) (caddr item))))))

;; folding
(eval-after-load 'folding
   '(progn
      (defface folding-header-line `((((class color) (min-colors 89))
				      (:foreground ,zenburn-green+4
						   :background ,zenburn-bg-1
						   :box (:line-width -1 :style released-button))))
	"Face for folding headers")
      (defvar folding-header-line-face 'folding-header-line)
      (setq folding-font-lock-begin-mark 'folding-header-line-face
	    folding-font-lock-end-mark   'folding-header-line-face)))

;; hideshowvis
(eval-after-load 'hideshowvis
  '(progn
     (set-face-attribute 'hideshowvis-hidable-face nil :foreground zenburn-green+3 :box nil)
     (set-face-attribute 'hs-fringe-face           nil :foreground zenburn-green+3 :box nil)
     (set-face-attribute 'hs-face                  nil :foreground zenburn-green+3 :background zenburn-bg-1 :box (list :style 'pressed-button))))
