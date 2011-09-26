;; -----------------------------------------------------------------------------
;; Emacs package manager :)
;; -----------------------------------------------------------------------------

(add-to-list 'load-path "~/.config/emacs/el-get/el-get")
(require 'el-get)

(setq el-get-dir "~/.config/emacs/el-get"
      el-get-recipe-path '("~/.config/emacs/el-get/el-get/recipes")
      el-get-sources
      '(deft doxymacs flyguess folding
	ioccur python-mode verbiste
	(:name magit
	       :url "git@github.com:Schnouki/magit.git")
	(:name undo-tree
	       :features undo-tree)
	))

(el-get)

(defun schnouki/el-get-update-all ()
  (interactive)
  (let (item name)
    (dolist (item el-get-sources)
      (setq name (symbol-name
		  (if (symbolp item) item
		    (plist-get item :name))))
      (message (concat "## Updating " name " ##"))
      (el-get-update name))))
