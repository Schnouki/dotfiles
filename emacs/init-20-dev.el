;;; 20-dev --- Development  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dash
  :ensure t)
(use-package list-utils
  :ensure t)
(use-package s
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
  :hook (after-init . global-flycheck-mode)
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
  :after flycheck
  :config
  (flycheck-pos-tip-mode t))

(use-package avy-flycheck
  :ensure t
  :bind (:map flycheck-command-map
              ("g" . avy-flycheck-goto-error)))

;; Jump to definition
(use-package xref
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package dumb-jump
  :ensure t
  :bind (:map schnouki-prefix-map
	 ("j j" . dumb-jump-go)
	 ("j o" . dumb-jump-go-other-window)
	 ("j p" . dumb-jump-back)
	 ("j i" . dumb-jump-go-prompt)
	 ("j l" . dumb-jump-quick-look)
         ("j J" . dumb-jump-go-prefer-external)
         ("j O" . dumb-jump-go-prefer-external-other-window))
  :commands (dumb-jump-xref-activate)
  :init
  (setq dumb-jump-selector 'ivy)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Default compilation commands
(setq-default compile-command "make") ;; I don't want "make -k"
(bind-key "c" 'compile schnouki-prefix-map)

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

;; Completion with LSP
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (prog-mode . lsp-deferred)
  :custom
  (lsp-auto-guess-root nil)
  (lsp-auto-select-workspace nil)
  (lsp-clients-emmy-lua-jar-path "/usr/lib/lua-emmy-language-server/EmmyLua-LS-all.jar")
  (lsp-enable-file-watchers nil)
  (lsp-prefer-flymake nil)
  :config
  (defun schnouki/lsp--disable-y-or-n-p (orig-fun &rest args)
    (if schnouki/desktop-was-read
	;; OK, proceed!
	(apply orig-fun args)
      ;; Still reading desktop time: turn y-or-no-p into a no-op
      (with-yes-or-no nil
	(apply orig-fun args))))
  (advice-add 'lsp :around #'schnouki/lsp--disable-y-or-n-p)
  (advice-add 'lsp--ask-about-watching-big-repo :around #'schnouki/lsp--disable-y-or-n-p))

(setq schnouki/desktop-was-read nil)

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-flycheck t)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-alignment 'window))

;; Use nimlsp from nimble
(with-eval-after-load-feature (lsp lsp-nim)
  (let ((client (gethash 'nimls lsp-clients)))
    (setf (lsp--client-new-connection client)
	  (lsp-stdio-connection "~/.nimble/bin/nimlsp"))))


;; Company -- complete anything
(use-package company
  :ensure t
  :diminish company-mode
  :hook (prog-mode . company-mode-on)
  :custom
  (company-tooltip-limit 30)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.5)
  (company-show-quick-access t)
  :config
  ;; TabNine and LSP fight against each other. To make them play well, keep them
  ;; separate. If that's not enough, the alternative is to use something like
  ;; https://github.com/karta0807913/emacs.d/blob/master/lisp/init-tabnine-capf.el.
  (setq company-backends (cons '(company-tabnine :separate company-capf)
                               (remove 'company-capf company-backends)))
  )

;; the all-language autocompleter
(use-package company-tabnine
  :ensure t
  :commands company-tabnine
  :custom
  (company-tabnine-binaries-folder "~/.local/share/TabNine"))


;; tree-sitter -- faster, fine-grained code highlighting, and much more
(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t)
(global-tree-sitter-mode 1)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

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
					(Javascript-mode . (("===" . ?≡)
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
(use-package restclient-jq)

;; Stop Unicode trolls (probably useless, hence commented out)
(use-package unicode-troll-stopper
  :disabled t
  :ensure t
  :commands unicode-troll-stopper-mode
  :diminish unicode-troll-stopper-mode
  :hook prog-mode)

;; Smart Comments
(use-package smart-comment
  :ensure t
  :bind ("M-;" . smart-comment))

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

;; So long, perf issues with minified code...
(when (require 'so-long nil :noerror)
  (global-so-long-mode 1))

;;; init-20-dev.el ends here
