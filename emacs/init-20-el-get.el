;; -----------------------------------------------------------------------------
;; Emacs package manager :)
;; -----------------------------------------------------------------------------

(add-to-list 'load-path "~/.config/emacs/el-get/el-get")
(require 'el-get)

(setq el-get-dir "~/.config/emacs/el-get"
      el-get-recipe-path '("~/.config/emacs/el-get/el-get/recipes")
      el-get-sources
      '(flyguess folding google-maps google-weather
	ioccur python-mode rainbow-mode verbiste
	(:name el-get
	       :url "git://github.com/Schnouki/el-get.git")
	(:name magit
	       :url "git://github.com/Schnouki/magit.git")
	(:name pymacs
	       :build ("make" "sudo make install"))
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
