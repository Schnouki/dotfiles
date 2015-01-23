;;; 20-dev --- Development
;;; Commentary:
;;; Code:

;; Tabs and indentation
(setq-default c-basic-offset 4
	      c-indent-level 4
	      indent-tabs-mode nil) ;; No tabs at all!

(use-package dtrt-indent
  :ensure dtrt-indent
  :commands dtrt-indent-mode
  :idle
  (dtrt-indent-mode 1))

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

(use-package dash
  :ensure dash)

;; Flycheck
(use-package flycheck
  :ensure flycheck
  :init
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode)

    (flycheck-define-checker python2-pylint
      "A Python syntax and style checker using Pylint2."
      :command ("pylint2" "-r" "n"
                "--msg-template" "{path}:{line}:{column}:{C}:{msg} ({msg_id})"
                (config-file "--rcfile" flycheck-pylint2rc)
                source-inplace)
      :error-patterns
      ((error line-start (file-name) ":" line ":" column ":"
	      (or "E" "F") ":" (message) line-end)
       (warning line-start (file-name) ":" line ":" column ":"
		(or "W" "R") ":" (message) line-end)
       (info line-start (file-name) ":" line ":" column ":"
	     "C:" (message) line-end))
      :modes python-mode)
    (flycheck-def-config-file-var flycheck-pylint2rc python2-pylint
				  ".pylintrc"
      :safe #'stringp)
    (add-to-list 'flycheck-checkers 'python2-pylint)))

(use-package flycheck-pos-tip
  :ensure flycheck-pos-tip
  :init
  (progn
    (setq flycheck-display-error-messages #'flycheck-pos-tip-error-messages)))

;; Code folding
(use-package folding
  :init
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
  :ensure hideshowvis
  :init
  (hideshowvis-enable))

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
  "*Specify the face to to use for the hidden region indicator."
  :type 'face
  :group 'hideshow)

(defface hs-face
  '((t (:background "#ff8" :box t)))
  "Face to hightlight the ... area of hidden regions"
  :group 'hideshow)

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
  :ensure company
  :init
  (progn
    (setq company-backends (remove 'company-ropemacs company-backends)
	  company-tooltip-limit 20)
    (global-company-mode 1)))

;; Fixmee
(use-package fixmee
  :ensure fixmee
  :idle
  (progn
    (setq button-lock-mode-lighter nil
	  fixmee-mode-lighter nil)
    (global-fixmee-mode 1)))

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
							("is" . ?≡)))
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
  :ensure expand-region
  :demand t
  :bind (("C-:" . er/expand-region)
	 ("C-," . er/contract-region)))

(use-package smart-forward
  :ensure smart-forward
  :bind (("M-<up>" . smart-up)
	 ("M-<down>" . smart-down)
	 ("M-<left>" . smart-backward)
	 ("M-<right>" . smart-forward)))

(use-package drag-stuff
  :ensure drag-stuff
  :init
  (progn
    (setq drag-stuff-modifier '(meta shift))
    (drag-stuff-global-mode t)))

;;; init-20-dev.el ends here
