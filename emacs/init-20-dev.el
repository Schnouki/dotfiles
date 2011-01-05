;; -----------------------------------------------------------------------------
;; Development 
;; -----------------------------------------------------------------------------

;; Tabs and indentation
(setq-default c-basic-offset 4
	      c-indent-level 4
	      indent-tabs-mode nil) ;; No tabs at all!

;; Default parameters for emacs-lisp
(defun schnouki/emacs-lisp-default-indent ()
  (setq indent-tabs-mode t))
(add-hook 'emacs-lisp-mode-hook 'schnouki/emacs-lisp-default-indent)

;; Shorter key bindings for folding/hideshow
(global-set-key (kbd "C-! :") '(lambda () (interactive)
				 (unless folding-mode (folding-mode))
				 (folding-toggle-show-hide)))
(global-set-key (kbd "C-! !") '(lambda () (interactive)
				 (unless hs-minor-mode (hs-minor-mode))
				 (hs-toggle-hiding)))

;; Default compilation commands
(setq-default compile-command "make") ;; I don't want "make -k"
(add-hook 'LaTeX-mode-hook (lambda () (setq compile-command "latexmk -pdf")))
(global-set-key (kbd "C-! c") 'compile)

;; Prevent ispell from verifying some LaTeX commands
;; http://stat.genopole.cnrs.fr/dw/~jchiquet/fr/latex/emacslatex
(setq schnouki/ispell-tex-skip-alists
      '("cite" "nocite"
	"includegraphics"
	"author" "affil"
	"ref" "eqref" "pageref"
	"label"))
(setq ispell-tex-skip-alists
      (list
       (append (car ispell-tex-skip-alists)
	       (mapcar '(lambda (cmd) (list (concat "\\\\" cmd) 'ispell-tex-arg-end)) schnouki/ispell-tex-skip-alists))
       (cadr ispell-tex-skip-alists)))

;; Indentation with align-current in LaTeX environments
(setq schnouki/LaTeX-align-environments '("tabular" "tabular*"))
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (require 'align)
	    (setq LaTeX-indent-environment-list
		  ;; For each item in the list...
		  (mapcar (lambda (item)
			    ;; The car is an environment
			    (let ((env (car item)))
			      ;; If this environment is in our list...
			      (if (member env schnouki/LaTeX-align-environments)
				  ;; ...then replace this item with a correct one
				  (list env 'align-current)
				;; else leave it alone
				item)))
			  LaTeX-indent-environment-list))))

;; Use dvipdfmx to convert DVI files to PDF in AUCTeX
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("DVI to PDF" "dvipdfmx %d" TeX-run-command t t) t))

;; Default scaling for preview-latex
(setq preview-scale 1.4)

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

;; Access the Common Lisp HyperSpec
;; http://clisp.cvs.sourceforge.net/*checkout*/clisp/clisp/emacs/clhs.el
;(require 'clhs)
;(global-set-key (kbd "C-h M-f") 'common-lisp-hyperspec)
