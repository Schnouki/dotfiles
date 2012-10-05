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

;; Display indicator in fringe for code that can be folded with hideshow
(hideshowvis-enable)

(define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])

(defcustom hs-fringe-face 'hs-fringe-face
  "*Specify face used to highlight the fringe on hidden regions."
  :type 'face
  :group 'hideshow)

(defface hs-fringe-face
  '((t (:foreground "#888" :box (:line-width 2 :color "grey75" :style released-button))))
  "Face used to highlight the fringe on folded regions"
  :group 'hideshow)

(defcustom hs-face 'hs-face
  "*Specify the face to to use for the hidden region indicator"
  :type 'face
  :group 'hideshow)

(defface hs-face
  '((t (:background "#ff8" :box t)))
  "Face to hightlight the ... area of hidden regions"
  :group 'hideshow)

(defun hs/display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string))
           (display-string (format "(%d)..." (count-lines (overlay-start ov) (overlay-end ov))))
           )
      (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
      (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 0 (length display-string) 'face 'hs-face display-string)
      (overlay-put ov 'display display-string)
      )))

(setq hs-set-up-overlay 'hs/display-code-line-counts)

;; Default compilation commands
(setq-default compile-command "make") ;; I don't want "make -k"
(global-set-key (kbd "C-! c") 'compile)
(global-set-key (kbd "C-ç c") 'compile)

;; Compilation: scroll the *compilation* buffer window as output appears, but
;; stop scrolling at the first error
(setq compilation-scroll-output 'first-error
      compilation-window-height nil)

;; Compilation: close the window when successful
;; from http://www.emacswiki.org/emacs/ModeCompile
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code) (not (string= mode-name "Ack")))
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
(dolist (mode '(python2-mode python3-mode coffee-mode))
  (add-to-list 'ac-modes mode))

;; Exuberant ctags
(require 'ctags-update)
(setq ctags-update-lighter " CU")
(add-hook 'prog-mode-hook (lambda () (ctags-auto-update-mode t)))
;; See also: https://gist.github.com/2901380

;; Fixmee
(require 'fixmee)
(setq button-lock-mode-lighter "")
(global-fixmee-mode 1)
