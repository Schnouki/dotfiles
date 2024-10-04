;;; 22-completion --- Completion
;;; Commentary:
;;; Code:

;; Vertico -- better UI for completion, based on built-in facilities
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)

  :custom
  (vertico-cycle nil)  ;; Enable cycling for `vertico-next/previous'

  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun schnouki/vertico--crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'schnouki/vertico--crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :hook (after-init . savehist-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; Consult -- better completing-read
(use-package consult
  :ensure t

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; Replace default bindings
  :bind (;; C-c bindings
         :map mode-specific-map
         ("M-x" . consult-mode-command)
         ("h" . consult-history)
         ("k" . consult-kmacro)
         ("m" . consult-man)
         ("i" . consult-info)
         ;; C-x bindings
         :map ctl-x-map
         ("M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("b" . consult-buffer)                ;; orig. switch-to-buffer
         ("4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; M-g bindings in `goto-map'
         :map goto-map
         ("e" . consult-compile-error)
         ("f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("g" . consult-goto-line)             ;; orig. goto-line
         ("M-g" . consult-goto-line)           ;; orig. goto-line
         ("o" . consult-outline)               ;; Alternative: consult-org-heading
         ("m" . consult-mark)
         ("k" . consult-global-mark)
         ("i" . consult-imenu)
         ("I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         :map search-map
         ("d" . consult-fd)                  ;; Alternative: consult-find
         ("c" . consult-locate)
         ("g" . consult-grep)
         ("G" . consult-git-grep)
         ("r" . consult-ripgrep)
         ("l" . consult-line)
         ("L" . consult-line-multi)
         ("k" . consult-keep-lines)
         ("u" . consult-focus-lines)
         ;; Isearch integration
         ("e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
         ;; Defaults, in global-map
         :map global-map
         ([remap Info-search] . consult-info)
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; ("C-s" . consult-line)                    ;; orig. isearch-forward -- similar to swiper
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register))

  :custom
  ;; Improve register preview
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  ;; Select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)

  (consult-narrow-key "<")
  (consult-project-function (lambda (_) (projectile-project-root)))

  :config
  ;; Register preview window
  (advice-add #'register-preview :override #'consult-register-window)
  )

;; Eglot integration
(use-package consult-eglot
  :ensure t)

;;; init-22-completion.el ends here
