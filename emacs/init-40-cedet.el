;; -----------------------------------------------------------------------------
;; CEDET 
;; -----------------------------------------------------------------------------

(require 'cedet)

;; Project management - EDE package
(global-ede-mode t)

;; Semantic
(require 'semantic)
(require 'semantic/complete)
(setq semantic-default-submodes semantic-submode-list)
(semantic-mode 1)

;; Settings for Semantic
(setq
 ;; Display completion candidates inline both for idle and C-<tab> completions
 semantic-complete-inline-analyzer-idle-displayor-class semantic-displayor-ghost
 semantic-complete-inline-analyzer-displayor-class semantic-displayor-ghost
 ;; Don't show idle parser messages in the minibuffer
 semantic-idle-scheduler-working-in-modeline-flag t)

;; Some key bindings
;; - Smart completion
(global-set-key (kbd "C-<tab>") 'semantic-complete-analyze-inline)
;; - Smart summary
(global-set-key (kbd "C-* h")   'semantic-ia-show-summary)
(global-set-key (kbd "C-* d")   'semantic-ia-show-doc)
(global-set-key (kbd "C-* C-d") 'semantic-ia-describe-class)
;; - Jump to the definition of a local tag
(global-set-key (kbd "C-* j")   'semantic-complete-jump-local)
;; - Jump to the definition of a tag in any file Emacs has parsed
(global-set-key (kbd "C-* J")   'semantic-complete-jump)
;; - Jump to recently edited tags
(global-set-key (kbd "C-* C-j") 'semantic-mrub-switch-tags)
;; - Find references for a tag
(global-set-key (kbd "C-* s")   'semantic-symref-symbol)
;; - Navigate to next/prev/parent tag
(global-set-key (kbd "C-* p")   'senator-previous-tag)
(global-set-key (kbd "C-* n")   'senator-next-tag)
(global-set-key (kbd "C-* u")   'senator-go-to-up-reference)
;; - Kill, copy, yank the current tag
(global-set-key (kbd "C-* C-w") 'senator-kill-tag)
(global-set-key (kbd "C-* M-w") 'senator-copy-tag)
(global-set-key (kbd "C-* C-y") 'senator-yank-tag)
;; - Transpose the current tag with the previous/next one
(global-set-key (kbd "C-* <up>")   'senator-transpose-tags-up)
(global-set-key (kbd "C-* <down>") 'senator-transpose-tags-down)

;; Mode-specific key bindings
(defun cedet-C-local-keys-hook ()
  (when (semantic-active-p)
    (local-set-key "." 'semantic-complete-self-insert)
    (local-set-key ">" 'semantic-complete-self-insert)))
(add-hook 'c-mode-common-hook 'cedet-C-local-keys-hook)

;; Python include path
(semantic-add-system-include "/usr/lib/python2.7/" 'python-mode)
(semantic-add-system-include "/usr/lib/python2.7/site-packages/" 'python-mode)
(setq semantic-python-dependency-system-include-path
      '("/usr/lib/python2.7/"
	"/usr/lib/python2.7/site-packages/"
	"/usr/lib/python3.2/"
	"/usr/lib/python3.2/site-packages/"))

;; -----------------------------------------------------------------------------
;; ECB -- Emacs Code Browser 
;; -----------------------------------------------------------------------------
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ecb")
(require 'ecb)

(setq ecb-layout-name "left6")

;; -----------------------------------------------------------------------------
;; Projects 
;; -----------------------------------------------------------------------------
(ede-cpp-root-project "OpenNL"
                :name "OpenNL project"
                :file "~/Recherche/ALICE/OpenNL/opennl/CMakeLists.txt"
                :include-path '("/src"))

(ede-cpp-root-project "Spop"
                :name "Spop project"
                :file "~/dev/spop/CMakeLists.txt"
                :include-path '("/src" "/plugin")
		:system-include-path '("/usr/include/dbus-1.0"
				       "/usr/include/glib-2.0"
				       "/usr/include/json-glib-1.0"
				       "/usr/include/libsoup-2.4"))

(ede-cpp-root-project "Gnome Commander"
		:name "Gnome Commander project"
		:file "~/dev/gcmd/autogen.sh"
		:include-path '("/"
				"/libgcmd"
				"/src"
				"/src/dialogs"
				"/src/interviewer"
				"/src/tags")
		:system-include-path '("/usr/include/gtk-2.0"
				       "/usr/include/libgnomeui-2.0"))
