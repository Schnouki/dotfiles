;;; 20-dev --- Development
;;; Commentary:
;;; Code:

(use-package dash
  :ensure t)

;; Tabs and indentation
(setq-default c-basic-offset 4
	      c-indent-level 4
	      indent-tabs-mode nil) ;; No tabs at all!

(use-package dtrt-indent
  :ensure t
  :commands dtrt-indent-mode
  :defer 3
  :config
  (progn
    (dtrt-indent-mode 1)
    (setq global-mode-string (--remove (eq it 'dtrt-indent-mode-line-info) global-mode-string))))

;; No trailing whitespaces
(defvar-local enable-delete-trailing-whitespace t
  "Enable or disable automatic deletion of trailing whitespace on a per-buffer basis.")

(defun schnouki/maybe-delete-trailing-whitespace ()
  "Delete trailing whitespace, unless `enable-delete-trailing-whitespace' is nil."
  (interactive)
  (when enable-delete-trailing-whitespace
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'schnouki/maybe-delete-trailing-whitespace)

;; Default parameters for emacs-lisp
(defun schnouki/emacs-lisp-default-indent ()
  "Fix default indent for emacs-lisp."
  (setq indent-tabs-mode t))
(add-hook 'emacs-lisp-mode-hook 'schnouki/emacs-lisp-default-indent)

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (progn
    (setq flycheck-check-syntax-automatically (--remove (or (eq it 'mode-enabled)
							    (eq it 'new-line))
							flycheck-check-syntax-automatically)))
  :init
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode)))

(use-package flycheck-pos-tip
  :ensure t
  :commands flycheck-pos-tip-error-messages
  :init
  (eval-after-load 'flycheck
    '(setq flycheck-display-error-messages #'flycheck-pos-tip-error-messages)))

;; Code folding
(use-package folding
  :diminish folding-mode
  :config
  (progn
    (setq folding-mode-prefix-key (kbd "C-:")
	  folding-folding-on-startup nil
	  folding-internal-margins nil)
    (folding-install)
    (folding-install-hooks)
    (add-hook 'after-revert-hook 'folding-mode-find-file t)))

;; Key bindings for hideshow
(defun schnouki/hs-togle-hiding ()
  "Toggle hideshow minor mode."
  (interactive)
  (unless hs-minor-mode (hs-minor-mode))
  (hs-toggle-hiding))
(bind-key "C-! !" 'schnouki/hs-togle-hiding)

;; Display indicator in fringe for code that can be folded with hideshow
(use-package hideshowvis
  :ensure t
  :config
  (progn
    (hideshowvis-enable)
    (hideshowvis-symbols)))

(defun hs/display-code-line-counts (ov)
  "Display the number of lines in a snippet hidden with hs."
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
(bind-key "C-! c" 'compile)

;; Compilation: scroll the *compilation* buffer window as output appears, but
;; stop scrolling at the first error
(setq compilation-scroll-output 'first-error
      compilation-window-height nil)

;; Compilation: close the window when successful
;; from http://www.emacswiki.org/emacs/ModeCompile
(defun compilation-exit-autoclose (status code msg)
  "Auto-close the compilation window when successful."
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

;; Company -- complete anything
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (progn
    (setq company-backends (remove 'company-ropemacs company-backends)
	  company-tooltip-limit 20)
    (global-company-mode 1)))

;; Fixmee
(use-package fixmee
  :ensure t
  :defer 10
  :config
  (progn
    (setq button-lock-mode-lighter nil
	  fixmee-mode-lighter nil)
    (require 'button-lock)
    (global-fixmee-mode)))

;; Display the current function name in the mode line
(which-function-mode 1)

;; Pretty symbols
;; http://emacsredux.com/blog/2014/08/25/a-peek-at-emacs-24-dot-4-prettify-symbols-mode/
(setq schnouki/prettify-symbols-common '(("lambda" . ?λ)
					 ("<=" . ?≤)
					 (">=" . ?≥)
					 ("!=" . ?≠)
					 ("-->" . ?→)
					 ("<--" . ?←))
      schnouki/prettify-symbols-modes '((emacs-lisp-mode . nil)
					(lisp-mode . nil)
					(python-mode . (("is not" . ?≢)
							("is" . ?≡)
							("in" . ?∈)
							("not in" . ?∉)))
					(javascript-mode . (("===" . ?≡)
							    ("!==" . ?≢)))))

(defun schnouki/maybe-enable-prettify-symbols-mode ()
  "Maybe enable prettify-symbol-mode for the current major mode."
  (let ((config (assq major-mode schnouki/prettify-symbols-modes)))
    (when config
      ;; Add common symbols to the alist
      (dolist (entry schnouki/prettify-symbols-common)
	(push entry prettify-symbols-alist))
      ;; Add mode-specific symbols to the alist
      (dolist (entry (cdr config))
	(push entry prettify-symbols-alist))
      ;; Enable prettify-symbols-mode
      (prettify-symbols-mode 1))))
(add-hook 'prog-mode-hook 'schnouki/maybe-enable-prettify-symbols-mode)

;; Increase selected region by semantic units
(use-package expand-region
  :ensure t
  :demand t
  :bind (("C-:" . er/expand-region)
	 ("C-," . er/contract-region)))

(use-package smart-forward
  :ensure t
  :bind (("M-<up>" . smart-up)
	 ("M-<down>" . smart-down)
	 ("M-<left>" . smart-backward)
	 ("M-<right>" . smart-forward)))

(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :config
  (progn
    (setq drag-stuff-modifier '(meta shift))
    (drag-stuff-global-mode t)))

;; REST client!
(use-package restclient
  :ensure t
  :commands restclient-mode)

;; Stop Unicode trolls (probably useless, hence commented out)
(use-package unicode-troll-stopper
  :disabled t
  :ensure t
  :commands unicode-troll-stopper-mode
  :diminish unicode-troll-stopper-mode
  :init
  (progn
    (add-hook 'prog-mode-hook #'unicode-troll-stopper-mode)))

;;; init-20-dev.el ends here
