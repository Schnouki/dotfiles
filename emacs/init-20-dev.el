;;; 20-dev --- Development  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dash
  :ensure t)
(use-package list-utils
  :ensure t)

;; Tabs and indentation
(setq-default c-basic-offset 4
	      c-indent-level 4
	      indent-tabs-mode nil) ;; No tabs at all!

(use-package dtrt-indent
  :ensure t
  :commands dtrt-indent-mode
  :diminish dtrt-indent-mode
  :defer 3
  :config
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

;; Flycheck
(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically (--remove (or (eq it 'mode-enabled)
                                                          (eq it 'new-line))
                                                      flycheck-check-syntax-automatically))
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
     :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
     :hint nil)
    "Flycheck errors"
    ("p" flycheck-previous-error "previous")
    ("n" flycheck-next-error "next")
    ("f" flycheck-error-list-set-filter "filter")
    ("P" flycheck-first-error "first")
    ("N" (progn (goto-char (point-max)) (flycheck-previous-error)) "last")
    ("h" flycheck-display-eror-at-point "display")
    ("e" flycheck-explain-error-at-point "explain")
    ("q" nil))
  (bind-key "!" 'hydra-flycheck/body flycheck-command-map))

(use-package flycheck-pos-tip
  :ensure t
  :commands flycheck-pos-tip-error-messages
  :init
  (eval-after-load 'flycheck
    '(setq flycheck-display-error-messages #'flycheck-pos-tip-error-messages)))

(use-package avy-flycheck
  :ensure t
  :bind (:map flycheck-command-map
              ("g" . avy-flycheck-goto-error)))

;; Jump to definition
(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)))

;; Code folding
(use-package origami
  :ensure t
  :commands origami-mode
  :bind (:map origami-mode-map
              ("C-: :" . origami-recursively-toggle-node)
              ("C-: a" . origami-toggle-all-nodes)
              ("C-: t" . origami-toggle-node)
              ("C-: o" . origami-show-only-node)
              ("C-: u" . origami-undo)
              ("C-: U" . origami-redo)
              ("C-: C-r" . origami-reset)
              )
  :config
  (setq origami-show-fold-header t)
  (let ((tbp (assoc 'triple-braces origami-parser-alist)))
    (setf (cdr tbp) #'schnouki/origami-triple-braces-parser))
  :init
  (setq schnouki/triple-braces-regex
        (rx line-start (0+ blank)
            (or "//" "/*" "#" "--" ";;")
            (0+ blank) (or (seq "{{{" (0+ any))
                           "}}}")))
  (defun schnouki/origami-triple-braces-parser (create)
    (lambda (content)
      (let ((positions (origami-get-positions content schnouki/triple-braces-regex))
            nodes)
        ;; Make it more readble: conver to a list of plists, each having :start,
        ;; :end, :offset and :children (which is a list with the same
        ;; structure).
        (cl-labels ((make-node ()
                               (let* ((start (car positions))
                                      (start-offset (s-index-of "{{{" (car start)))
                                      (start-pos (+ (cdr start) start-offset))
                                      (offset (- (length (car start)) start-offset))
                                      (next (cadr positions))
                                      (node `(:start ,start-pos :offset ,offset))
                                      children)
                                 (!cdr positions)
                                 (while (s-contains? "{{{" (car next))
                                   (setq children (cons (make-node) children)
                                         next (car positions)))
                                 (plist-put node :end (+ (cdr next) (s-index-of "}}}" (car next))))
                                 (when children
                                   (plist-put node :children (reverse children)))
                                 (!cdr positions)
                                 node))
                    (build (nodes)
                           (--map (funcall create
                                           (plist-get it :start)
                                           (plist-get it :end)
                                           (plist-get it :offset)
                                           (build (plist-get it :children)))
                                  nodes)))
          (while positions
            (setq nodes (cons (make-node) nodes)))
          (build (reverse nodes))))))
  (defun schnouki/enable-origami-mode ()
    "Enable origami-mode, and set the fold-style to 'triple-braces' if it makes sense."
    (when (and (not (local-variable-p 'origami-fold-style))
               (save-mark-and-excursion
                (goto-char (point-min))
                (search-forward "{{{" nil t)))
      (message "Enabling triple braces folding with Origami")
      (setq-local origami-fold-style 'triple-braces))
    (origami-mode 1))
  (add-hook 'prog-mode-hook 'schnouki/enable-origami-mode))

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
  (when (and
	 (eq status 'exit)
	 (zerop code)
	 (not (-contains? '("Ag" "Ripgrep") mode-name)))
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
  (setq company-backends (remove 'company-ropemacs company-backends)
        company-tooltip-limit 20
        company-tooltip-align-annotations t)
  (global-company-mode 1))

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
;(add-hook 'prog-mode-hook 'schnouki/maybe-enable-prettify-symbols-mode)

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

(use-package move-text
  :ensure t
  :commands (move-text-up move-text-down))

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

;; Smart Comments
(use-package smart-comment
  :ensure t
  :bind ("M-;" . smart-comment))

;; GNU Global
(use-package ggtags
  :ensure t)

;; Automatically make some buffers read-only
(use-package auto-read-only
  :ensure t
  :diminish auto-read-only-mode
  :config
  (--each (list (rx "/.config/emacs/elpa/")
                (rx "/vendor/")
                (rx "/" (or ".virtualenvs" "venv" ".venv") "/"))
    (add-to-list 'auto-read-only-file-regexps it))
  (auto-read-only-mode 1))

;; Highlight TODO and similar keywords
(use-package hl-todo
  :ensure t
  :defer t
  :commands global-hl-todo-mode
  :bind (:map hl-todo-mode-map
	      ("C-! h p" . hl-todo-previous)
	      ("C-! h n" . hl-todo-next)
	      ("C-! h o" . hl-todo-occur))
  :config
  (global-hl-todo-mode))

;;; init-20-dev.el ends here

;; Local Variables:
;; origami-fold-style: triple-braces
;; End:
