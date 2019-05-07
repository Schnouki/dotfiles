;;; 10-base --- Basic settings
;;; Commentary:
;;; Code:

;; Tweak the GC threshold.
;; The default value is 800000, set it to something higher once the startup is
;; complete. And set it to something even higher in the minibuffer (for ido and
;; other complex features)
(setq schnouki/gc-threshold 2000000
      schnouki/gc-threshold-minibuffer 10000000)

(defun schnouki/set-normal-gc-threshold ()
  (setq gc-cons-threshold schnouki/gc-threshold))
(defun schnouki/set-minibuffer-gc-threshold ()
  (setq gc-cons-threshold schnouki/gc-threshold-minibuffer))

(add-hook 'after-init-hook #'schnouki/set-normal-gc-threshold)
(add-hook 'minibuffer-setup-hook #'schnouki/set-minibuffer-gc-threshold)
(add-hook 'minibuffer-exit-hook #'schnouki/set-normal-gc-threshold)

(setq gc-cons-threshold most-positive-fixnum)

;; Paths
(add-to-list 'load-path "~/.config/emacs")

;; Fix for tramp "recursive load"
;; (http://lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00064.html)
(when (< emacs-major-version 24)
  (load "/usr/share/emacs/23.3/lisp/net/tramp.el.gz"))

;; Custom file
(setq custom-file "~/.config/emacs/init-00-custom.el")

;; Keep all backup files in a single directory
(setq backup-directory-alist '(("." . "~/.emacs-backup-files/")))

;; Web browser
(setq browse-url-browser-function 'browse-url-firefox  ;chromium
      browse-url-firefox-program "firefox-developer-edition"
      browse-url-firefox-new-window-is-tab t)

;; Display date and time
(require 'time)
(display-time)
(setq display-time-24hr-format t)

;; Display line and colon number
(column-number-mode t)
(line-number-mode t)

;; No menu bar, tool bar, scroll bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; No internal border
(add-to-list 'default-frame-alist '(internal-border . 0))

;; Focus follows mouse
;(setq mouse-autoselect-window t)

;; No beep, but flash screen
(setq visible-bell t)

;; Move point to top/bottom of buffer before signalling a scrolling error
(setq scroll-error-top-bottom t)

;; Display file name in the window title bar
(setq frame-title-format '(buffer-file-name "%b [%f]" "%b"))

;; Answer "y" rather than "yes"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use "initials" completion style
(add-to-list 'completion-styles 'initials t)

;; Avoid sentences that end with 2 spaces (American style).
;; TODO: change this automatically according to the current dictionary
(setq sentence-end-double-space nil)

;; Avoid breaking lines at '(' or ':' characters
(add-hook 'fill-no-break-predicate 'fill-french-nobreak-p)

;; Justify at 80 columns
(setq-default fill-column 80)

;; Display matching parenthesis
;; http://emacs-fu.blogspot.com/2009/01/balancing-your-parentheses.html
(require 'paren)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Show the matching parenthseis when it is offscreen
;; http://www.emacswiki.org/emacs/ShowParenMode#toc1
(defun schnouki/show-matching-paren-offscreen ()
  "If the matching paren is offscreen, show the matching line in the echo area.
Has no effect if the character before point is not of the syntax class ')'."
  (let* ((cb (char-before (point)))
	 (matching-text (and cb
			     (char-equal (char-syntax cb) ?\) )
			     (blink-matching-open))))
    (when matching-text (message matching-text))))
(advice-add 'show-paren-function :after #'schnouki/show-matching-paren-offscreen)

;; Highlight current line
;; http://www.emacsblog.org/2007/04/09/highlight-the-current-line/
(global-hl-line-mode 1)

;; Case-insensitive search
(setq case-fold-search t)

;; Better selection behaviour
(if (< emacs-major-version 24)
    (pc-selection-mode 1)
  (progn
    (delete-selection-mode 1)
    (setq x-select-enable-clipboard t
	  x-select-enable-primary   t)))

;; Highlight current region
(transient-mark-mode t)

;; Active region becomes the window selection
(setq select-active-regions t)

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
;; http://pragmaticemacs.com/emacs/add-the-system-clipboard-to-the-emacs-kill-ring/
(setq save-interprogram-paste-before-kill t)

;; ediff window setup
;; - don't open a new frame for the control buffer
;; - split horizontally if the current frame is wide enough
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function (lambda (&optional arg)
				    (if (> (frame-width) 150)
					(split-window-horizontally arg)
				      (split-window-vertically arg))))
;; - restore window configuration when quitting ediff
(defvar ediff-saved-window-configuration nil)
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
(require 'desktop)
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
  "Kill the minibuffer."
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
(defvar schnouki/no-script nil)
(defun schnouki/maybe-make-executable-if-script-p ()
  "Automagically make scripts executable."
  (let ((name (buffer-file-name)))
    (unless (cl-some #'(lambda (dir) (string-prefix-p (expand-file-name dir) name))
		     schnouki/no-script)
      (executable-make-buffer-file-executable-if-script-p))))
(add-hook 'after-save-hook 'schnouki/maybe-make-executable-if-script-p)

;; Wait a very little bit before fontifying buffers
;; http://tsengf.blogspot.fr/2012/11/slow-scrolling-speed-in-emacs.html
;(setq jit-lock-defer-time 0.05)

;; Better naming than main.yml<2>, main.yml<3>, main.yml<4>
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;  Move through camelCaseWords and other_long_words
(global-subword-mode 1)

;; Enable "confusing" commands
(dolist (feat '(downcase-region upcase-region))
  (put feat 'disabled nil))

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; init-10-base.el ends here
