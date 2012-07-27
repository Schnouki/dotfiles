;; -----------------------------------------------------------------------------
;; Development
;; -----------------------------------------------------------------------------

;; Tabs and indentation
(setq-default c-basic-offset 4
	      c-indent-level 4
	      indent-tabs-mode nil) ;; No tabs at all!

;; No trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Default parameters for emacs-lisp
(defun schnouki/emacs-lisp-default-indent ()
  (setq indent-tabs-mode t))
(add-hook 'emacs-lisp-mode-hook 'schnouki/emacs-lisp-default-indent)

;; Code folding
(require 'folding)

;; Shorter key bindings for folding/hideshow
(dolist (key (list (kbd "C-! :") (kbd "C-ç f")))
  (global-set-key key '(lambda () (interactive)
			 (unless folding-mode (folding-mode))
			 (folding-toggle-show-hide))))
(dolist (key (list (kbd "C-! !") (kbd "C-ç ç")))
  (global-set-key key '(lambda () (interactive)
			 (unless hs-minor-mode (hs-minor-mode))
			 (hs-toggle-hiding))))

;; Default compilation commands
(setq-default compile-command "make") ;; I don't want "make -k"
(global-set-key (kbd "C-! c") 'compile)
(global-set-key (kbd "C-ç c") 'compile)

;; Compilation: scroll the *compilation* buffer window as output appears, but
;; stop scrolling at the first error
(setq compilation-scroll-output t
      compilation-window-height 8)

;; Compilation: close the window when successful
;; from http://www.emacswiki.org/emacs/ModeCompile
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;; Make ediff fr_FR-locale-friendly
;(eval-after-load 'ediff-diff
;  '(setq ediff-diff-ok-lines-regexp
;	(concat (substring ediff-diff-ok-lines-regexp 0 -2) "\\|.*Pas de fin de ligne\\)")))

;; Auto-Complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-sources 'ac-source-gtags)
(dolist (mode '(python2-mode python3-mode))
  (add-to-list 'ac-modes mode))
