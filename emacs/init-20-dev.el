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

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
  	 ("\\.mdwn\\'" . markdown-mode)
  	 ("\\.markdown\\'" . markdown-mode)))

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
  :limit-process t
  :custom
  (lsp-auto-guess-root nil)
  (lsp-auto-select-workspace nil)
  (lsp-clients-emmy-lua-jar-path "/usr/lib/lua-emmy-language-server/EmmyLua-LS-all.jar")
  (lsp-enable-file-watchers nil)
  (lsp-prefer-flymake nil)
  (lsp-warn-no-matched-clients nil)
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
  :limit-process t
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
	  (lsp-stdio-connection "~/.local/share/nimble/bin/nimlsp"))))

;; https://github.com/abo-abo/hydra/wiki/lsp-mode
(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
^^-------------------- ^^------------------------ --------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))
(bind-key "l" 'hydra-lsp/body schnouki-prefix-map)


;; DAP - Debug Adapter Protocol
(use-package dap-mode
  :ensure t
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (dap-auto-configure-mode t)
  (dap-register-debug-template "Python :: Attach to running process"
			       (list :type "python"
				     :request "attach"
				     :connect (list :host "localhost"
						    :port 5679))))

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
  (company-show-quick-access t))


;; treesit-auto -- automatically use tree-sitter whenever possible
;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  (treesit-auto-install t)
  :config
  (global-treesit-auto-mode 1))

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
					(python-base-mode . (("is not" . ?≢)
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

;; ElDoc
(use-package eldoc
  :hook (emacs-lisp-mode ielm-mode lisp-interaction-mode lisp-mode python-mode))

;; Paredit
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode eval-expression-minibuffer-setup ielm-mode
			  lisp-interaction-mode lisp-mode scheme-mode) . enable-paredit-mode)
  :config
  ;; Don't touch my C-<left> and C-<right>!
  (dolist (key '("C-<right>" "C-<left>" "ESC C-<right>" "ESC C-<left>"))
    (unbind-key key paredit-mode-map))
  (bind-keys :map paredit-mode-map
	     ("C-M-<right>" . paredit-forward-slurp-sexp)
	     ("C-M-<left>" . paredit-forward-barf-sexp)
	     ("C-S-<left>" . paredit-backward-slurp-sexp)
	     ("C-S-<right>" . paredit-backward-barf-sexp))
  (eval-after-load 'eldoc
    (eldoc-add-command 'paredit-backward-delete
		       'paredit-close-round)))

;; REST client!
(use-package restclient
  :ensure t
  :commands restclient-mode)
(use-package restclient-jq
  :ensure t)

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

;; Full-featured terminal emulator
(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
	      ("C-q" . vterm-send-next-key)
	 :map schnouki-prefix-map
	      ("v v" . vterm)
	      ("v o" . vterm-other-window))
  :hook ((vterm-mode . #'schnouki/disable-hl-line-mode-locally)
	 (vterm-copy-mode . #'schnouki/enable-hl-line-mode)))

;;; init-20-dev.el ends here
