;; -----------------------------------------------------------------------------
;; CEDET 
;; -----------------------------------------------------------------------------

(require 'cedet)

;; Project management - EDE package
(global-ede-mode t)

;; Semantic
(require 'semantic)
(setq semantic-default-submodes semantic-submode-list)
(semantic-mode 1)

;; Some key bindings
;; - Completion
(global-set-key (kbd "C-<tab>") 'semantic-ia-complete-symbol-menu)
;; - Go to prototype
(global-set-key (kbd "C-² C-p") 'semantic-analyze-proto-impl-toggle)
;; - Jump to the tag refered at POINT (and back again)
(global-set-key (kbd "C-² j") 'semantic-ia-fast-jump)
(global-set-key (kbd "C-² b") 'semantic-mrub-switch-tags)
;; - Navigate to next/prev/parent tag
(global-set-key (kbd "C-² n") 'senator-next-tag)
(global-set-key (kbd "C-² n") 'senator-previous-tag)
(global-set-key (kbd "C-² u") 'senator-go-to-up-reference)
;; - Search where a function is called
(global-set-key (kbd "C-² s") 'semantic-symref)
(global-set-key (kbd "C-² C-s") 'semantic-symref-symbol)

;; Mode-specific key bindings
(defun cedet-C-local-keys-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert))
(add-hook 'c-mode-common-hook 'cedet-C-local-keys-hook)


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
				       "/usr/include/libsoup-2.4"))

(ede-cpp-root-project "Gnome Commander"
		:name "Gnome Commander project"
		:file "~/dev/gcmd/Makefile"
		:include-path '("/"
				"/libgcmd"
				"/src"
				"/src/dialogs"
				"/src/interviewer"
				"/src/tags")
		:system-include-path '("/usr/include/gtk-2.0"
				       "/usr/include/libgnomeui-2.0"))
