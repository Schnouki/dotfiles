;; -----------------------------------------------------------------------------
;; Basic settings
;; -----------------------------------------------------------------------------

;; Paths
(add-to-list 'load-path "~/.config/emacs")

;; Fix for tramp "recursive load"
;; (http://lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00064.html)
(load "/usr/share/emacs/23.3/lisp/net/tramp.el.gz")

;; Custom file
(setq custom-file "~/.config/emacs/init-00-custom.el")

;; Keep all backup files in a single directory
(setq backup-directory-alist '(("." . "~/.emacs-backup-files/")))

;; Default font
(add-to-list 'default-frame-alist
	     (if (string-prefix-p "odin" (system-name))
		 '(font ."Mono-7")
	       '(font . "Mono-11")))

;; Web browser
(setq browse-url-browser-function 'browse-url-firefox)

;; Display date and time
(display-time)
(setq display-time-24hr-format t)

;; Display line and colon number
(column-number-mode t)
(line-number-mode t)

;; No menu bar, tool bar, scroll bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Focus follows mouse
;(setq mouse-autoselect-window t)

;; No beep, but flash screen
(setq visible-bell t)

;; Display file name in the window title bar
(setq frame-title-format '(buffer-file-name "%b [%f]" "%b"))

;; Answer "y" rather than "yes"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Display possible completions in minibuffer
;; http://www.emacswiki.org/emacs/IcompleteMode
;; TODO: test Icicles...
(icomplete-mode 99)

;; Use "initials" completion style
(add-to-list 'completion-styles 'initials t)

;; Display matching parenthesis
;; http://emacs-fu.blogspot.com/2009/01/balancing-your-parentheses.html
(require 'paren)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Show the matching parenthseis when it is offscreen
;; http://www.emacswiki.org/emacs/ShowParenMode#toc1
(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the                               
    echo area. Has no effect if the character before point is not of                                   
    the syntax class ')'."
  (interactive)
  (let ((matching-text nil))
    ;; Only call `blink-matching-open' if the character before point                               
    ;; is a close parentheses type character. Otherwise, there's not                               
    ;; really any point, and `blink-matching-open' would just echo                                 
    ;; "Mismatched parentheses", which gets really annoying.                                       
    (if (char-equal (char-syntax (char-before (point))) ?\))
	(setq matching-text (blink-matching-open)))
    (if (not (null matching-text))
	(message matching-text))))


;; Highlight current line
;; http://www.emacsblog.org/2007/04/09/highlight-the-current-line/
(global-hl-line-mode 1)

;; Case-insensitive search
(setq case-fold-search t)

;; Cut/copy/paste with S-delete, C-insert and S-insert
(pc-selection-mode 1)

;; Highlight current region
(transient-mark-mode t)

;; Active region becomes the window selection
(setq select-active-region t)

;; ediff window setup
;; - don't open a new frame for the control buffer
;; - split horizontally if the current frame is wide enough
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function (lambda (&optional arg)
				    (if (> (frame-width) 150)
					(split-window-horizontally arg)
				      (split-window-vertically arg))))
;; - restore window configuration when quitting ediff
(add-hook 'ediff-load-hook
	  (lambda ()
	    (add-hook 'ediff-before-setup-hook
		      (lambda ()
			(setq ediff-saved-window-configuration (current-window-configuration))))
	    (let ((restore-window-configuration
		   (lambda ()
		     (set-window-configuration ediff-saved-window-configuration))))
	      (add-hook 'ediff-quit-hook restore-window-configuration 'append)
	      (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))

;; Remember the last visited line in a file
(setq-default save-place t)
(require 'saveplace)
(setq save-place-file "~/.config/emacs/places")

;; Save opened files and other stuff
;; http://www.xsteve.at/prg/emacs/power-user-tips.html
(setq desktop-save t
      desktop-load-locked-desktop t
      desktop-path '("." "~/.config/emacs"))
(desktop-save-mode 1)
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))
;; http://www.emacswiki.org/emacs/DeskTop
(add-hook 'auto-save-hook 'desktop-save-in-desktop-dir)

;; Abort the minibuffer when using the mouse
;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (>= (recursion-depth) 1)
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Don't ask if I want to kill a buffer with a live process attached to it
;; http://www.masteringemacs.org/articles/2010/11/14/disabling-prompts-emacs/
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

;; Automagically make scripts executable
;; http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; ido-mode for better buffer switching, file selection, etc.
(require 'ido)
(ido-mode 1)
(setq ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)
